------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 1                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Casing;   use Casing;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Util; use Exp_Util;
with Hostparm; use Hostparm;
with Inline;   use Inline;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Uname;    use Uname;

package body Exp_Ch11 is

   SD_List : List_Id;
   --  This list gathers the values SDn'Unrestricted_Access used to
   --  construct the unit exception table. It is set to Empty_List if
   --  there are no subprogram descriptors.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Exception_Handler_Tables (HSS : Node_Id);
   --  Subsidiary procedure called by Expand_Exception_Handlers if zero
   --  cost exception handling is installed for this target. Replaces the
   --  exception handler structure with appropriate labeled code and tables
   --  that allow the zero cost exception handling circuits to find the
   --  correct handler (see unit Ada.Exceptions for details).

   procedure Generate_Subprogram_Descriptor
     (N     : Node_Id;
      Loc   : Source_Ptr;
      Spec  : Entity_Id;
      Slist : List_Id);
   --  Procedure called to generate a subprogram descriptor. N is the
   --  subprogram body node or, in the case of an imported subprogram, is
   --  Empty, and Spec is the entity of the sunprogram. For details of the
   --  required structure, see package System.Exceptions. The generated
   --  subprogram descriptor is appended to Slist. Loc provides the
   --  source location to be used for the generated descriptor.

   ---------------------------
   -- Expand_At_End_Handler --
   ---------------------------

   --  For a handled statement sequence that has a cleanup (At_End_Proc
   --  field set), an exception handler of the following form is required:

   --     exception
   --       when all others =>
   --          cleanup call
   --          raise;

   --  Note: this exception handler is treated rather specially by
   --  subsequent expansion in two respects:

   --    The normal call to Undefer_Abort is omitted
   --    The raise call does not do Defer_Abort

   --  This is because the current tasking code seems to assume that
   --  the call to the cleanup routine that is made from an exception
   --  handler for the abort signal is called with aborts deferred.

   procedure Expand_At_End_Handler (HSS : Node_Id; Block : Node_Id) is
      Clean   : constant Entity_Id  := Entity (At_End_Proc (HSS));
      Loc     : constant Source_Ptr := Sloc (Clean);
      Ohandle : Node_Id;
      Stmnts  : List_Id;

   begin
      pragma Assert (Present (Clean));
      pragma Assert (No (Exception_Handlers (HSS)));

      if Restrictions (No_Exception_Handlers) then
         return;
      end if;

      if Present (Block) then
         New_Scope (Block);
      end if;

      Ohandle :=
        Make_Others_Choice (Loc);
      Set_All_Others (Ohandle);

      Stmnts := New_List (
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Clean, Loc)),
        Make_Raise_Statement (Loc));

      Set_Exception_Handlers (HSS, New_List (
        Make_Exception_Handler (Loc,
          Exception_Choices => New_List (Ohandle),
          Statements        => Stmnts)));

      Analyze_List (Stmnts, Suppress => All_Checks);
      Expand_Exception_Handlers (HSS);

      if Present (Block) then
         Pop_Scope;
      end if;
   end Expand_At_End_Handler;

   -------------------------------------
   -- Expand_Exception_Handler_Tables --
   -------------------------------------

   --  See Ada.Exceptions specification for full details of the data
   --  structures that we need to construct here. As an example of the
   --  transformation that is required, given the structure:

   --     declare
   --        {declarations}
   --        ..
   --     begin
   --        {statements-1}
   --        ...
   --     exception
   --        when a | b =>
   --           {statements-2}
   --           ...
   --        when others =>
   --           {statements-3}
   --           ...
   --     end;

   --  We transform this into:

   --     declare
   --        {declarations}
   --        ...
   --        L1 : label;
   --        L2 : label;
   --        L3 : label;
   --        L4 : Label;
   --        L5 : label;

   --     begin
   --        <<L1>>
   --           {statements-1}
   --        <<L2>>

   --     exception

   --        when a | b =>
   --           <<L3>>
   --           {statements-2}

   --           HR2 : constant Handler_Record := (
   --                   Lo      => L1'Address,
   --                   Hi      => L2'Address,
   --                   Id      => a'Identity,
   --                   Handler => L5'Address);

   --           HR3 : constant Handler_Record := (
   --                   Lo      => L1'Address,
   --                   Hi      => L2'Address,
   --                   Id      => b'Identity,
   --                   Handler => L4'Address);

   --        when others =>
   --           <<L4>>
   --           {statements-3}

   --           HR1 : constant Handler_Record := (
   --                   Lo      => L1'Address,
   --                   Hi      => L2'Address,
   --                   Id      => Others_Id,
   --                   Handler => L4'Address);
   --     end;

   --  The exception handlers in the transformed version are marked with the
   --  Zero_Cost_Handling flag set, and all gigi does in this case is simply
   --  to put the handler code somewhere. It can optionally be put inline
   --  between the goto L3 and the label <<L3>> (which is why we generate
   --  that goto in the first place).

   procedure Expand_Exception_Handler_Tables (HSS : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (HSS);
      Handlrs : constant List_Id    := Exception_Handlers (HSS);
      Stms    : constant List_Id    := Statements (HSS);
      Handler : Node_Id;

      Hlist : List_Id;
      --  This is the list to which handlers are to be appended. It is
      --  either the list for the enclosing subprogram, or the enclosing
      --  selective accept statement (which will turn into a subprogram
      --  during expansion later on).

      L1 : constant Entity_Id :=
             Make_Defining_Identifier (Loc,
               Chars => New_Internal_Name ('L'));

      L2 : constant Entity_Id :=
             Make_Defining_Identifier (Loc,
               Chars => New_Internal_Name ('L'));

      Lnn    : Entity_Id;
      Choice : Node_Id;
      E_Id   : Node_Id;
      HR_Ent : Node_Id;
      HL_Ref : Node_Id;
      Item   : Node_Id;

      Subp_Entity : Entity_Id;
      --  This is the entity for the subprogram (or library level package)
      --  to which the handler record is to be attached for later reference
      --  in a subprogram descriptor for this entity.

      procedure Append_To_Stms (N : Node_Id);
      --  Append given statement to the end of the statements of the
      --  handled sequence of statements and analyze it in place.

      function Inside_Selective_Accept return Boolean;
      --  This function is called if we are inside the scope of an entry
      --  or task. It checks if the handler is appearing in the context
      --  of a selective accept statement. If so, Hlist is set to
      --  temporarily park the handlers in the N_Accept_Alternative.
      --  node. They will subsequently be moved to the procedure entity
      --  for the procedure built for this alternative. The statements that
      --  follow the Accept within the alternative are not inside the Accept
      --  for purposes of this test, and handlers that may appear within
      --  them belong in the enclosing task procedure.

      procedure Set_Hlist;
      --  Sets the handler list corresponding to Subp_Entity

      --------------------
      -- Append_To_Stms --
      --------------------

      procedure Append_To_Stms (N : Node_Id) is
      begin
         Insert_After_And_Analyze (Last (Stms), N);
         Set_Exception_Junk (N);
      end Append_To_Stms;

      -----------------------------
      -- Inside_Selective_Accept --
      -----------------------------

      function Inside_Selective_Accept return Boolean is
         Parnt : Node_Id;
         Curr  : Node_Id := HSS;

      begin
         Parnt := Parent (HSS);
         while Nkind (Parnt) /= N_Compilation_Unit loop
            if Nkind (Parnt) = N_Accept_Alternative
              and then Curr = Accept_Statement (Parnt)
            then
               if Present (Accept_Handler_Records (Parnt)) then
                  Hlist := Accept_Handler_Records (Parnt);
               else
                  Hlist := New_List;
                  Set_Accept_Handler_Records (Parnt, Hlist);
               end if;

               return True;
            else
               Curr  := Parnt;
               Parnt := Parent (Parnt);
            end if;
         end loop;

         return False;
      end Inside_Selective_Accept;

      ---------------
      -- Set_Hlist --
      ---------------

      procedure Set_Hlist is
      begin
         --  Never try to inline a subprogram with exception handlers

         Set_Is_Inlined (Subp_Entity, False);

         if Present (Subp_Entity)
           and then Present (Handler_Records (Subp_Entity))
         then
            Hlist := Handler_Records (Subp_Entity);
         else
            Hlist := New_List;
            Set_Handler_Records (Subp_Entity, Hlist);
         end if;
      end Set_Hlist;

   --  Start of processing for Expand_Exception_Handler_Tables

   begin
      --  Nothing to do if this handler has already been processed

      if Zero_Cost_Handling (HSS) then
         return;
      end if;

      Set_Zero_Cost_Handling (HSS);

      --  Find the parent subprogram or package scope containing this
      --  exception frame. This should always find a real package or
      --  subprogram. If it does not it will stop at Standard, but
      --  this cannot legitimately occur.

      --  We only stop at library level packages, for inner packages
      --  we always attach handlers to the containing procedure.

      Subp_Entity := Current_Scope;
      Scope_Loop : loop

         --  Never need tables expanded inside a generic template

         if Is_Generic_Unit (Subp_Entity) then
            return;

         --  Stop if we reached containing subprogram. Go to protected
         --  subprogram if there is one defined.

         elsif Ekind (Subp_Entity) = E_Function
           or else Ekind (Subp_Entity) = E_Procedure
         then
            if Present (Protected_Body_Subprogram (Subp_Entity)) then
               Subp_Entity := Protected_Body_Subprogram (Subp_Entity);
            end if;

            Set_Hlist;
            exit Scope_Loop;

         --  Case of within an entry

         elsif Is_Entry (Subp_Entity) then

            --  Protected entry, use corresponding body subprogram

            if Present (Protected_Body_Subprogram (Subp_Entity)) then
               Subp_Entity := Protected_Body_Subprogram (Subp_Entity);
               Set_Hlist;
               exit Scope_Loop;

            --  Check if we are within a selective accept alternative

            elsif Inside_Selective_Accept then

               --  As a side effect, Inside_Selective_Accept set Hlist,
               --  in much the same manner as Set_Hlist, except that
               --  the list involved was the one for the selective accept.

               exit Scope_Loop;
            end if;

         --  Case of within library level package

         elsif Ekind (Subp_Entity) = E_Package
           and then Is_Compilation_Unit (Subp_Entity)
         then
            if Is_Body_Name (Unit_Name (Get_Code_Unit (HSS))) then
               Subp_Entity := Body_Entity (Subp_Entity);
            end if;

            Set_Hlist;
            exit Scope_Loop;

         --  Task type case

         elsif Ekind (Subp_Entity) = E_Task_Type then

            --  Check if we are within a selective accept alternative

            if Inside_Selective_Accept then

               --  As a side effect, Inside_Selective_Accept set Hlist,
               --  in much the same manner as Set_Hlist, except that the
               --  list involved was the one for the selective accept.

               exit Scope_Loop;

            --  Stop if we reached task type with task body procedure,
            --  use the task body procedure.

            elsif Present (Get_Task_Body_Procedure (Subp_Entity)) then
               Subp_Entity := Get_Task_Body_Procedure (Subp_Entity);
               Set_Hlist;
               exit Scope_Loop;
            end if;
         end if;

         --  If we fall through, keep looking

         Subp_Entity := Scope (Subp_Entity);
      end loop Scope_Loop;

      pragma Assert (Subp_Entity /= Standard_Standard);

      --  Analyze standard labels

      Analyze_Label_Entity (L1);
      Analyze_Label_Entity (L2);

      Insert_Before_And_Analyze (First (Stms),
        Make_Label (Loc,
          Identifier => New_Occurrence_Of (L1, Loc)));
      Set_Exception_Junk (First (Stms));

      Append_To_Stms (
        Make_Label (Loc,
          Identifier => New_Occurrence_Of (L2, Loc)));

      --  Loop through exception handlers

      Handler := First_Non_Pragma (Handlrs);
      while Present (Handler) loop
         Set_Zero_Cost_Handling (Handler);

         --  Add label at start of handler, and goto at the end

         Lnn :=
           Make_Defining_Identifier (Loc,
             Chars => New_Internal_Name ('L'));

         Analyze_Label_Entity (Lnn);

         Item :=
           Make_Label (Loc,
             Identifier => New_Occurrence_Of (Lnn, Loc));
         Set_Exception_Junk (Item);
         Insert_Before_And_Analyze (First (Statements (Handler)), Item);

         --  Loop through choices

         Choice := First (Exception_Choices (Handler));
         while Present (Choice) loop

            --  Others (or all others) choice

            if Nkind (Choice) = N_Others_Choice then
               if All_Others (Choice) then
                  E_Id := New_Occurrence_Of (RTE (RE_All_Others_Id), Loc);
               else
                  E_Id := New_Occurrence_Of (RTE (RE_Others_Id), Loc);
               end if;

            --  Special case of VMS_Exception. Not clear what we will do
            --  eventually here if and when we implement zero cost exceptions
            --  on VMS. But at least for now, don't blow up trying to take
            --  a garbage code address for such an exception.

            elsif Is_VMS_Exception (Entity (Choice)) then
               E_Id := New_Occurrence_Of (RTE (RE_Null_Id), Loc);

            --  Normal case of specific exception choice

            else
               E_Id :=
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Entity (Choice), Loc),
                   Attribute_Name => Name_Identity);
            end if;

            HR_Ent :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('H'));

            HL_Ref :=
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (HR_Ent, Loc),
                Attribute_Name => Name_Unrestricted_Access);

            --  Now we need to add the entry for the new handler record to
            --  the list of handler records for the current subprogram.

            --  Normally we end up generating the handler records in exactly
            --  the right order. Here right order means innermost first,
            --  since the table will be searched sequentially. Since we
            --  generally expand from outside to inside, the order is just
            --  what we want, and we need to append the new entry to the
            --  end of the list.

            --  However, there are exceptions, notably in the case where
            --  a generic body is inserted later on. See for example the
            --  case of ACVC test C37213J, which has the following form:

            --    generic package x ... end x;
            --    package body x is
            --    begin
            --       ...
            --    exception  (1)
            --       ...
            --    end x;

            --    ...

            --    declare
            --       package q is new x;
            --    begin
            --       ...
            --    exception (2)
            --       ...
            --    end;

            --  In this case, we will expand exception handler (2) first,
            --  since the expansion of (1) is delayed till later when the
            --  generic body is inserted. But (1) belongs before (2) in
            --  the chain.

            --  Note that scopes are not totally ordered, because two
            --  scopes can be in parallel blocks, so that it does not
            --  matter what order these entries appear in. An ordering
            --  relation exists if one scope is inside another, and what
            --  we really want is some partial ordering.

            --  A simple, not very efficient, but adequate algorithm to
            --  achieve this partial ordering is to search the list for
            --  the first entry containing the given scope, and put the
            --  new entry just before it.

            declare
               New_Scop : constant Entity_Id := Current_Scope;
               Ent      : Node_Id;

            begin
               Ent := First (Hlist);
               loop
                  --  If all searched, then we can just put the new
                  --  entry at the end of the list (it actually does
                  --  not matter where we put it in this case).

                  if No (Ent) then
                     Append_To (Hlist, HL_Ref);
                     exit;

                  --  If the current scope is within the scope of the
                  --  entry then insert the entry before to retain the
                  --  proper order as per above discussion.

                  --  Note that for equal entries, we just keep going,
                  --  which is fine, the entry will end up at the end
                  --  of the list where it belongs.

                  elsif Scope_Within
                          (New_Scop, Scope (Entity (Prefix (Ent))))
                  then
                     Insert_Before (Ent, HL_Ref);
                     exit;

                  --  Otherwise keep looking

                  else
                     Next (Ent);
                  end if;
               end loop;
            end;

            Item :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => HR_Ent,
                Constant_Present    => True,
                Aliased_Present     => True,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Handler_Record), Loc),

                Expression          =>
                  Make_Aggregate (Loc,
                    Expressions => New_List (
                      Make_Attribute_Reference (Loc,             -- Lo
                        Prefix => New_Occurrence_Of (L1, Loc),
                        Attribute_Name => Name_Address),

                      Make_Attribute_Reference (Loc,             -- Hi
                        Prefix => New_Occurrence_Of (L2, Loc),
                        Attribute_Name => Name_Address),

                      E_Id,                                      -- Id

                      Make_Attribute_Reference (Loc,
                        Prefix => New_Occurrence_Of (Lnn, Loc),  -- Handler
                        Attribute_Name => Name_Address))));

            Set_Handler_List_Entry (Item, HL_Ref);
            Set_Exception_Junk (Item);
            Insert_After_And_Analyze (Last (Statements (Handler)), Item);
            Set_Is_Statically_Allocated (HR_Ent);

            --  If this is a late insertion (from body instance) it is being
            --  inserted in the component list of an already analyzed aggre-
            --  gate, and must be analyzed explicitly.

            Analyze_And_Resolve (HL_Ref, RTE (RE_Handler_Record_Ptr));

            Next (Choice);
         end loop;

         Next_Non_Pragma (Handler);
      end loop;
   end Expand_Exception_Handler_Tables;

   -------------------------------
   -- Expand_Exception_Handlers --
   -------------------------------

   procedure Expand_Exception_Handlers (HSS : Node_Id) is
      Handlrs       : constant List_Id := Exception_Handlers (HSS);
      Loc           : Source_Ptr;
      Handler       : Node_Id;
      Others_Choice : Boolean;
      Obj_Decl      : Node_Id;

      procedure Prepend_Call_To_Handler
        (Proc : RE_Id;
         Args : List_Id := No_List);
      --  Routine to prepend a call to the procedure referenced by Proc at
      --  the start of the handler code for the current Handler.

      -----------------------------
      -- Prepend_Call_To_Handler --
      -----------------------------

      procedure Prepend_Call_To_Handler
        (Proc : RE_Id;
         Args : List_Id := No_List)
      is
         Ent : constant Entity_Id := RTE (Proc);

      begin
         --  If we have no Entity, then we are probably in no run time mode
         --  or some weird error has occured. In either case do do nothing!

         if Present (Ent) then
            declare
               Call : constant Node_Id :=
                        Make_Procedure_Call_Statement (Loc,
                          Name => New_Occurrence_Of (RTE (Proc), Loc),
                          Parameter_Associations => Args);

            begin
               Prepend_To (Statements (Handler), Call);
               Analyze (Call, Suppress => All_Checks);
            end;
         end if;
      end Prepend_Call_To_Handler;

   --  Start of processing for Expand_Exception_Handlers

   begin
      --  Loop through handlers

      Handler := First_Non_Pragma (Handlrs);
      while Present (Handler) loop
         Loc := Sloc (Handler);

         --  If an exception occurrence is present, then we must declare it
         --  and initialize it from the value stored in the TSD

         --     declare
         --        name : Exception_Occurrence;
         --
         --     begin
         --        Save_Occurrence (name, Get_Current_Excep.all)
         --        ...
         --     end;

         if Present (Choice_Parameter (Handler)) then
            declare
               Cparm : constant Entity_Id  := Choice_Parameter (Handler);
               Clc   : constant Source_Ptr := Sloc (Cparm);
               Save  : Node_Id;

            begin
               Save :=
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_Save_Occurrence), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Cparm, Clc),
                     Make_Explicit_Dereference (Loc,
                       Make_Function_Call (Loc,
                         Name => Make_Explicit_Dereference (Loc,
                           New_Occurrence_Of
                             (RTE (RE_Get_Current_Excep), Loc))))));

               Mark_Rewrite_Insertion (Save);
               Prepend (Save, Statements (Handler));

               Obj_Decl :=
                 Make_Object_Declaration (Clc,
                   Defining_Identifier => Cparm,
                   Object_Definition   =>
                     New_Occurrence_Of
                       (RTE (RE_Exception_Occurrence), Clc));
               Set_No_Initialization (Obj_Decl, True);

               Rewrite (Handler,
                 Make_Exception_Handler (Loc,
                   Exception_Choices => Exception_Choices (Handler),

                   Statements => New_List (
                     Make_Block_Statement (Loc,
                       Declarations => New_List (Obj_Decl),
                       Handled_Statement_Sequence =>
                         Make_Handled_Sequence_Of_Statements (Loc,
                           Statements => Statements (Handler))))));

               Analyze_List (Statements (Handler), Suppress => All_Checks);
            end;
         end if;

         --  The processing at this point is rather different for the
         --  JVM case, so we completely separate the processing.

         --  For the JVM case, we unconditionally call Update_Exception,
         --  passing a call to the intrinsic function Current_Target_Exception
         --  (see JVM version of Ada.Exceptions in 4jexcept.adb for details).

         if Hostparm.Java_VM then
            declare
               Arg  : Node_Id
                 := Make_Function_Call (Loc,
                      Name => New_Occurrence_Of
                                (RTE (RE_Current_Target_Exception), Loc));
            begin
               Prepend_Call_To_Handler (RE_Update_Exception, New_List (Arg));
            end;

         --  For the normal case, we have to worry about the state of abort
         --  deferral. Generally, we defer abort during runtime handling of
         --  exceptions. When control is passed to the handler, then in the
         --  normal case we undefer aborts. In any case this entire handling
         --  is relevant only if aborts are allowed!

         elsif Abort_Allowed then

            --  There are some special cases in which we do not do the
            --  undefer. In particular a finalization (AT END) handler
            --  wants to operate with aborts still deferred.

            --  We also suppress the call if this is the special handler
            --  for Abort_Signal, since if we are aborting, we want to keep
            --  aborts deferred (one abort is enough thank you very much :-)

            --  If abort really needs to be deferred the expander must add
            --  this call explicitly, see Exp_Ch9.Expand_N_Asynchronous_Select.

            Others_Choice :=
              Nkind (First (Exception_Choices (Handler))) = N_Others_Choice;

            if (Others_Choice
                 or else Entity (First (Exception_Choices (Handler))) /=
                                                      Stand.Abort_Signal)
              and then not
                (Others_Choice
                   and then All_Others (First (Exception_Choices (Handler))))
              and then Abort_Allowed
            then
               Prepend_Call_To_Handler (RE_Abort_Undefer);
            end if;
         end if;

         Next_Non_Pragma (Handler);
      end loop;

      --  The last step for expanding exception handlers is to expand the
      --  exception tables if zero cost exception handling is active.

      if Exception_Mechanism = Front_End_ZCX then
         Expand_Exception_Handler_Tables (HSS);
      end if;
   end Expand_Exception_Handlers;

   ------------------------------------
   -- Expand_N_Exception_Declaration --
   ------------------------------------

   --  Generates:
   --     exceptE : constant String := "A.B.EXCEP";   -- static data
   --     except : exception_data :=  (
   --                    Handled_By_Other => False,
   --                    Lang             => 'A',
   --                    Name_Length      => exceptE'Length
   --                    Full_Name        => exceptE'Address
   --                    HTable_Ptr       => null);

   --  (protecting test only needed if not at library level)
   --
   --     exceptF : Boolean := True --  static data
   --     if exceptF then
   --        exceptF := False;
   --        Register_Exception (except'Unchecked_Access);
   --     end if;

   procedure Expand_N_Exception_Declaration (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Id      : constant Entity_Id  := Defining_Identifier (N);
      L       : List_Id             := New_List;
      Flag_Id : Entity_Id;

      Name_Exname : constant Name_Id := New_External_Name (Chars (Id), 'E');
      Exname      : constant Node_Id :=
                      Make_Defining_Identifier (Loc, Name_Exname);

   begin
      --  There is no expansion needed when compiling for the JVM since the
      --  JVM has a built-in exception mechanism. See 4jexcept.ads for details.

      if Hostparm.Java_VM then
         return;
      end if;

      --  Definition of the external name: nam : constant String := "A.B.NAME";

      Insert_Action (N,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Exname,
          Constant_Present    => True,
          Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
          Expression => Make_String_Literal (Loc, Full_Qualified_Name (Id))));

      Set_Is_Statically_Allocated (Exname);

      --  Create the aggregate list for type Standard.Exception_Type:
      --  Handled_By_Other component: False

      Append_To (L, New_Occurrence_Of (Standard_False, Loc));

      --  Lang component: 'A'

      Append_To (L,
        Make_Character_Literal (Loc, Name_uA, Get_Char_Code ('A')));

      --  Name_Length component: Nam'Length

      Append_To (L,
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Exname, Loc),
          Attribute_Name => Name_Length));

      --  Full_Name component: Standard.A_Char!(Nam'Address)

      Append_To (L, Unchecked_Convert_To (Standard_A_Char,
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Exname, Loc),
          Attribute_Name => Name_Address)));

      --  HTable_Ptr component: null

      Append_To (L, Make_Null (Loc));

      --  Import_Code component: 0

      Append_To (L, Make_Integer_Literal (Loc, 0));

      Set_Expression (N, Make_Aggregate (Loc, Expressions => L));
      Analyze_And_Resolve (Expression (N), Etype (Id));

      --  Register_Exception (except'Unchecked_Access);

      if not Restrictions (No_Exception_Handlers) then
         L := New_List (
                Make_Procedure_Call_Statement (Loc,
                  Name => New_Occurrence_Of (RTE (RE_Register_Exception), Loc),
                  Parameter_Associations => New_List (
                    Unchecked_Convert_To (RTE (RE_Exception_Data_Ptr),
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (Id, Loc),
                        Attribute_Name => Name_Unrestricted_Access)))));

         Set_Register_Exception_Call (Id, First (L));

         if not Is_Library_Level_Entity (Id) then
            Flag_Id :=  Make_Defining_Identifier (Loc,
                          New_External_Name (Chars (Id), 'F'));

            Insert_Action (N,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Flag_Id,
                Object_Definition   =>
                  New_Occurrence_Of (Standard_Boolean, Loc),
                Expression          =>
                  New_Occurrence_Of (Standard_True, Loc)));

            Set_Is_Statically_Allocated (Flag_Id);

            Append_To (L,
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Flag_Id, Loc),
                Expression => New_Occurrence_Of (Standard_False, Loc)));

            Insert_After_And_Analyze (N,
              Make_Implicit_If_Statement (N,
                Condition       => New_Occurrence_Of (Flag_Id, Loc),
                Then_Statements => L));

         else
            Insert_List_After_And_Analyze (N, L);
         end if;
      end if;

   end Expand_N_Exception_Declaration;

   ---------------------------------------------
   -- Expand_N_Handled_Sequence_Of_Statements --
   ---------------------------------------------

   procedure Expand_N_Handled_Sequence_Of_Statements (N : Node_Id) is
   begin
      if Present (Exception_Handlers (N))
        and then not Restrictions (No_Exception_Handlers)
      then
         Expand_Exception_Handlers (N);
      end if;

      --  The following code needs comments ???

      if Nkind (Parent (N)) /= N_Package_Body
        and then Nkind (Parent (N)) /= N_Accept_Statement
        and then not Delay_Cleanups (Current_Scope)
      then
         Expand_Cleanup_Actions (Parent (N));
      else
         Set_First_Real_Statement (N, First (Statements (N)));
      end if;

   end Expand_N_Handled_Sequence_Of_Statements;

   -------------------------------------
   -- Expand_N_Raise_Constraint_Error --
   -------------------------------------

   --  The only processing required is to adjust the condition to deal
   --  with the C/Fortran boolean case. This may well not be necessary,
   --  as all such conditions are generated by the expander and probably
   --  are all standard boolean, but who knows what strange optimization
   --  in future may require this adjustment!

   procedure Expand_N_Raise_Constraint_Error (N : Node_Id) is
   begin
      Adjust_Condition (Condition (N));
   end Expand_N_Raise_Constraint_Error;

   ----------------------------------
   -- Expand_N_Raise_Program_Error --
   ----------------------------------

   --  The only processing required is to adjust the condition to deal
   --  with the C/Fortran boolean case. This may well not be necessary,
   --  as all such conditions are generated by the expander and probably
   --  are all standard boolean, but who knows what strange optimization
   --  in future may require this adjustment!

   procedure Expand_N_Raise_Program_Error (N : Node_Id) is
   begin
      Adjust_Condition (Condition (N));
   end Expand_N_Raise_Program_Error;

   ------------------------------
   -- Expand_N_Raise_Statement --
   ------------------------------

   procedure Expand_N_Raise_Statement (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Ehand : Node_Id;
      E     : Entity_Id;
      Str   : String_Id;

   begin
      --  There is no expansion needed for statement "raise <exception>;" when
      --  compiling for the JVM since the JVM has a built-in exception
      --  mechanism. However we need the keep the expansion for "raise;"
      --  statements. See 4jexcept.ads for details.

      if Present (Name (N)) and then Hostparm.Java_VM then
         return;
      end if;

      --  Convert explicit raise of Program_Error, Constraint_Error, and
      --  Storage_Error into the corresponding raise node (in No_Run_Time
      --  mode all other raises will get normal expansion and be disallowed,
      --  but this is also faster in all modes).

      if Present (Name (N)) and then Nkind (Name (N)) = N_Identifier then
         if Entity (Name (N)) = Standard_Constraint_Error then
            Rewrite (N,
              Make_Raise_Constraint_Error (Loc,
                Reason => CE_Explicit_Raise));
            Analyze (N);
            return;

         elsif Entity (Name (N)) = Standard_Program_Error then
            Rewrite (N,
              Make_Raise_Program_Error (Loc,
                Reason => PE_Explicit_Raise));
            Analyze (N);
            return;

         elsif Entity (Name (N)) = Standard_Storage_Error then
            Rewrite (N,
              Make_Raise_Storage_Error (Loc,
                Reason => SE_Explicit_Raise));
            Analyze (N);
            return;
         end if;
      end if;

      --  Case of name present, in this case we expand raise name to

      --    Raise_Exception (name'Identity, location_string);

      --  where location_string identifies the file/line of the raise

      if Present (Name (N)) then
         declare
            Id : Entity_Id := Entity (Name (N));

         begin
            Build_Location_String (Loc);

            --  If the exception is a renaming, use the exception that it
            --  renames (which might be a predefined exception, e.g.).

            if Present (Renamed_Object (Id)) then
               Id := Renamed_Object (Id);
            end if;

            --  Build a C compatible string in case of no exception handlers,
            --  since this is what the last chance handler is expecting.

            if Restrictions (No_Exception_Handlers) then
               --  Generate a C null message when Global_Discard_Names is True
               --  or when Debug_Flag_NN is set.

               if Global_Discard_Names or else Debug_Flag_NN then
                  Name_Buffer (1) := ASCII.NUL;
                  Name_Len := 1;
               else
                  Name_Len := Name_Len + 1;
               end if;

            --  Do not generate the message when Global_Discard_Names is True
            --  or when Debug_Flag_NN is set.

            elsif Global_Discard_Names or else Debug_Flag_NN then
               Name_Len := 0;
            end if;

            Str := String_From_Name_Buffer;

            --  For VMS exceptions, convert the raise into a call to
            --  lib$stop so it will be handled by __gnat_error_handler.

            if Is_VMS_Exception (Id) then
               declare
                  Excep_Image : String_Id;
                  Cond        : Node_Id;

               begin
                  if Present (Interface_Name (Id)) then
                     Excep_Image := Strval (Interface_Name (Id));
                  else
                     Get_Name_String (Chars (Id));
                     Set_All_Upper_Case;
                     Excep_Image := String_From_Name_Buffer;
                  end if;

                  if Exception_Code (Id) /= No_Uint then
                     Cond :=
                       Make_Integer_Literal (Loc, Exception_Code (Id));
                  else
                     Cond :=
                       Unchecked_Convert_To (Standard_Integer,
                         Make_Function_Call (Loc,
                           Name => New_Occurrence_Of
                             (RTE (RE_Import_Value), Loc),
                           Parameter_Associations => New_List
                             (Make_String_Literal (Loc,
                               Strval => Excep_Image))));
                  end if;

                  Rewrite (N,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Lib_Stop), Loc),
                      Parameter_Associations => New_List (Cond)));
                        Analyze_And_Resolve (Cond, Standard_Integer);
               end;

            --  Not VMS exception case, convert raise to call to the
            --  Raise_Exception routine.

            else
               Rewrite (N,
                 Make_Procedure_Call_Statement (Loc,
                    Name => New_Occurrence_Of (RTE (RE_Raise_Exception), Loc),
                    Parameter_Associations => New_List (
                      Make_Attribute_Reference (Loc,
                        Prefix => Name (N),
                        Attribute_Name => Name_Identity),
                      Make_String_Literal (Loc,
                        Strval => Str))));
            end if;
         end;

      --  Case of no name present (reraise). We rewrite the raise to:

      --    Reraise_Occurrence_Always (EO);

      --  where EO is the current exception occurrence. If the current handler
      --  does not have a choice parameter specification, then we provide one.

      else
         --  Find innermost enclosing exception handler (there must be one,
         --  since the semantics has already verified that this raise statement
         --  is valid, and a raise with no arguments is only permitted in the
         --  context of an exception handler.

         Ehand := Parent (N);
         while Nkind (Ehand) /= N_Exception_Handler loop
            Ehand := Parent (Ehand);
         end loop;

         --  Make exception choice parameter if none present. Note that we do
         --  not need to put the entity on the entity chain, since no one will
         --  be referencing this entity by normal visibility methods.

         if No (Choice_Parameter (Ehand)) then
            E := Make_Defining_Identifier (Loc, New_Internal_Name ('E'));
            Set_Choice_Parameter (Ehand, E);
            Set_Ekind (E, E_Variable);
            Set_Etype (E, RTE (RE_Exception_Occurrence));
            Set_Scope (E, Current_Scope);
         end if;

         --  Now rewrite the raise as a call to Reraise. A special case arises
         --  if this raise statement occurs in the context of a handler for
         --  all others (i.e. an at end handler). in this case we avoid
         --  the call to defer abort, cleanup routines are expected to be
         --  called in this case with aborts deferred.

         declare
            Ech : constant Node_Id := First (Exception_Choices (Ehand));
            Ent : Entity_Id;

         begin
            if Nkind (Ech) = N_Others_Choice
              and then All_Others (Ech)
            then
               Ent := RTE (RE_Reraise_Occurrence_No_Defer);
            else
               Ent := RTE (RE_Reraise_Occurrence_Always);
            end if;

            Rewrite (N,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (Ent, Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Choice_Parameter (Ehand), Loc))));
         end;
      end if;

      Analyze (N);
   end Expand_N_Raise_Statement;

   ----------------------------------
   -- Expand_N_Raise_Storage_Error --
   ----------------------------------

   --  The only processing required is to adjust the condition to deal
   --  with the C/Fortran boolean case. This may well not be necessary,
   --  as all such conditions are generated by the expander and probably
   --  are all standard boolean, but who knows what strange optimization
   --  in future may require this adjustment!

   procedure Expand_N_Raise_Storage_Error (N : Node_Id) is
   begin
      Adjust_Condition (Condition (N));
   end Expand_N_Raise_Storage_Error;

   ------------------------------
   -- Expand_N_Subprogram_Info --
   ------------------------------

   procedure Expand_N_Subprogram_Info (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  For now, we replace an Expand_N_Subprogram_Info node with an
      --  attribute reference that gives the address of the procedure.
      --  This is because gigi does not yet recognize this node, and
      --  for the initial targets, this is the right value anyway.

      Rewrite (N,
        Make_Attribute_Reference (Loc,
          Prefix => Identifier (N),
          Attribute_Name => Name_Code_Address));

      Analyze_And_Resolve (N, RTE (RE_Code_Loc));
   end Expand_N_Subprogram_Info;

   ------------------------------------
   -- Generate_Subprogram_Descriptor --
   ------------------------------------

   procedure Generate_Subprogram_Descriptor
     (N     : Node_Id;
      Loc   : Source_Ptr;
      Spec  : Entity_Id;
      Slist : List_Id)
   is
      Code  : Node_Id;
      Ent   : Entity_Id;
      Decl  : Node_Id;
      Dtyp  : Entity_Id;
      Numh  : Nat;
      Sdes  : Node_Id;
      Hrc   : List_Id;

   begin
      if Exception_Mechanism /= Front_End_ZCX then
         return;
      end if;

      if Restrictions (No_Exception_Handlers) then
         return;
      end if;

      --  Suppress descriptor if we are not generating code. This happens
      --  in the case of a -gnatc -gnatt compilation where we force generics
      --  to be generated, but we still don't want exception tables.

      if Operating_Mode /= Generate_Code then
         return;
      end if;

      --  Suppress descriptor if we are in No_Exceptions restrictions mode,
      --  since we can never propagate exceptions in any case in this mode.
      --  The same consideration applies for No_Exception_Handlers (which
      --   is also set in No_Run_Time mode).

      if Restrictions (No_Exceptions)
        or Restrictions (No_Exception_Handlers)
      then
         return;
      end if;

      --  Suppress descriptor if we are inside a generic. There are two
      --  ways that we can tell that, depending on what is going on. If
      --  we are actually inside the processing for a generic right now,
      --  then Expander_Active will be reset. If we are outside the
      --  generic, then we will see the generic entity.

      if not Expander_Active then
         return;
      end if;

      --  Suppress descriptor is subprogram is marked as eliminated, for
      --  example if this is a subprogram created to analyze a default
      --  expression with potential side effects. Ditto if it is nested
      --  within an eliminated subprogram, for example a cleanup action.

      declare
         Scop : Entity_Id;

      begin
         Scop := Spec;
         while Scop /= Standard_Standard loop
            if Ekind (Scop) = E_Generic_Procedure
                 or else
               Ekind (Scop) = E_Generic_Function
                 or else
               Ekind (Scop) = E_Generic_Package
                 or else
               Is_Eliminated (Scop)
            then
               return;
            end if;

            Scop := Scope (Scop);
         end loop;
      end;

      --  Suppress descriptor for original protected subprogram (we will
      --  be called again later to generate the descriptor for the actual
      --  protected body subprogram.) This does not apply to barrier
      --  functions which are there own protected subprogram.

      if Is_Subprogram (Spec)
        and then Present (Protected_Body_Subprogram (Spec))
        and then Protected_Body_Subprogram (Spec) /= Spec
      then
         return;
      end if;

      --  Suppress descriptors for packages unless they have at least one
      --  handler. The binder will generate the dummy (no handler) descriptors
      --  for elaboration procedures. We can't do it here, because we don't
      --  know if an elaboration routine does in fact exist.

      --  If there is at least one handler for the package spec or body
      --  then most certainly an elaboration routine must exist, so we
      --  can safely reference it.

      if (Nkind (N) = N_Package_Declaration
            or else
          Nkind (N) = N_Package_Body)
        and then No (Handler_Records (Spec))
      then
         return;
      end if;

      --  Suppress all subprogram descriptors for the file System.Exceptions.
      --  We similarly suppress subprogram descriptors for Ada.Exceptions.
      --  These are all init_proc's for types which cannot raise exceptions.
      --  The reason this is done is that otherwise we get embarassing
      --  elaboration dependencies.

      Get_Name_String (Unit_File_Name (Current_Sem_Unit));

      if Name_Buffer (1 .. 12) = "s-except.ads"
           or else
         Name_Buffer (1 .. 12) = "a-except.ads"
      then
         return;
      end if;

      --  Similarly, we need to suppress entries for System.Standard_Library,
      --  since otherwise we get elaboration circularities. Again, this would
      --  better be done with a Suppress_Initialization pragma :-)

      if Name_Buffer (1 .. 11) = "s-stalib.ad" then
         return;
      end if;

      --  For now, also suppress entries for s-stoele because we have
      --  some kind of unexplained error there ???

      if Name_Buffer (1 .. 11) = "s-stoele.ad" then
         return;
      end if;

      --  And also for g-htable, because it cannot raise exceptions,
      --  and generates some kind of elaboration order problem.

      if Name_Buffer (1 .. 11) = "g-htable.ad" then
         return;
      end if;

      --  Suppress subprogram descriptor if already generated. This happens
      --  in the case of late generation from Delay_Subprogram_Descriptors
      --  beging set (where there is more than one instantiation in the list)

      if Has_Subprogram_Descriptor (Spec) then
         return;
      else
         Set_Has_Subprogram_Descriptor (Spec);
      end if;

      --  Never generate descriptors for inlined bodies

      if Analyzing_Inlined_Bodies then
         return;
      end if;

      --  Here we definitely are going to generate a subprogram descriptor

      declare
         Hnum : Nat := Homonym_Number (Spec);

      begin
         if Hnum = 1 then
            Hnum := 0;
         end if;

         Ent :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Spec), "SD", Hnum));
      end;

      if No (Handler_Records (Spec)) then
         Hrc  := Empty_List;
         Numh := 0;
      else
         Hrc  := Handler_Records (Spec);
         Numh := List_Length (Hrc);
      end if;

      New_Scope (Spec);

      --  We need a static subtype for the declaration of the subprogram
      --  descriptor. For the case of 0-3 handlers we can use one of the
      --  predefined subtypes in System.Exceptions. For more handlers,
      --  we build our own subtype here.

      case Numh is
         when 0 =>
            Dtyp := RTE (RE_Subprogram_Descriptor_0);

         when 1 =>
            Dtyp := RTE (RE_Subprogram_Descriptor_1);

         when 2 =>
            Dtyp := RTE (RE_Subprogram_Descriptor_2);

         when 3 =>
            Dtyp := RTE (RE_Subprogram_Descriptor_3);

         when others =>
            Dtyp :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('T'));

            --  Set the constructed type as global, since we will be
            --  referencing the object that is of this type globally

            Set_Is_Statically_Allocated (Dtyp);

            Decl :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => Dtyp,
                Subtype_Indication =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      New_Occurrence_Of (RTE (RE_Subprogram_Descriptor), Loc),
                    Constraint =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints => New_List (
                          Make_Integer_Literal (Loc, Numh)))));

            Append (Decl, Slist);

            --  We analyze the descriptor for the subprogram and package
            --  case, but not for the imported subprogram case (it will
            --  be analyzed when the freeze entity actions are analyzed.

            if Present (N) then
               Analyze (Decl);
            end if;

            Set_Exception_Junk (Decl);
      end case;

      --  Prepare the code address entry for the table entry. For the normal
      --  case of being within a procedure, this is simply:

      --    P'Code_Address

      --  where P is the procedure, but for the package case, it is

      --    P'Elab_Body'Code_Address
      --    P'Elab_Spec'Code_Address

      --  for the body and spec respectively. Note that we do our own
      --  analysis of these attribute references, because we know in this
      --  case that the prefix of ELab_Body/Spec is a visible package,
      --  which can be referenced directly instead of using the general
      --  case expansion for these attributes.

      if Ekind (Spec) = E_Package then
         Code :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Spec, Loc),
             Attribute_Name => Name_Elab_Spec);
         Set_Etype (Code, Standard_Void_Type);
         Set_Analyzed (Code);

      elsif Ekind (Spec) = E_Package_Body then
         Code :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Spec_Entity (Spec), Loc),
             Attribute_Name => Name_Elab_Body);
         Set_Etype (Code, Standard_Void_Type);
         Set_Analyzed (Code);

      else
         Code := New_Occurrence_Of (Spec, Loc);
      end if;

      Code :=
        Make_Attribute_Reference (Loc,
          Prefix         => Code,
          Attribute_Name => Name_Code_Address);

      Set_Etype (Code, RTE (RE_Address));
      Set_Analyzed (Code);

      --  Now we can build the subprogram descriptor

      Sdes :=
        Make_Object_Declaration (Loc,
          Defining_Identifier      => Ent,
          Constant_Present         => True,
          Aliased_Present          => True,
          Object_Definition        => New_Occurrence_Of (Dtyp, Loc),

          Expression               =>
            Make_Aggregate (Loc,
              Expressions => New_List (
                Make_Integer_Literal (Loc, Numh),          -- Num_Handlers

                Code,                                      -- Code

--  temp code ???

--                Make_Subprogram_Info (Loc,                 -- Subprogram_Info
--                  Identifier =>
--                    New_Occurrence_Of (Spec, Loc)),

                New_Copy_Tree (Code),

                Make_Aggregate (Loc,                       -- Handler_Records
                  Expressions => Hrc))));

      Set_Exception_Junk (Sdes);
      Set_Is_Subprogram_Descriptor (Sdes);

      Append (Sdes, Slist);

      --  We analyze the descriptor for the subprogram and package case,
      --  but not for the imported subprogram case (it will be analyzed
      --  when the freeze entity actions are analyzed.

      if Present (N) then
         Analyze (Sdes);
      end if;

      --  We can now pop the scope used for analyzing the descriptor

      Pop_Scope;

      --  We need to set the descriptor as statically allocated, since
      --  it will be referenced from the unit exception table.

      Set_Is_Statically_Allocated (Ent);

      --  Append the resulting descriptor to the list. We do this only
      --  if we are in the main unit. You might think that we could
      --  simply skip generating the descriptors completely if we are
      --  not in the main unit, but in fact this is not the case, since
      --  we have problems with inconsistent serial numbers for internal
      --  names if we do this.

      if In_Extended_Main_Code_Unit (Spec) then
         Append_To (SD_List,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Ent, Loc),
             Attribute_Name => Name_Unrestricted_Access));

         Unit_Exception_Table_Present := True;
      end if;

   end Generate_Subprogram_Descriptor;

   ------------------------------------------------------------
   -- Generate_Subprogram_Descriptor_For_Imported_Subprogram --
   ------------------------------------------------------------

   procedure Generate_Subprogram_Descriptor_For_Imported_Subprogram
     (Spec  : Entity_Id;
      Slist : List_Id)
   is
   begin
      Generate_Subprogram_Descriptor (Empty, Sloc (Spec), Spec, Slist);
   end Generate_Subprogram_Descriptor_For_Imported_Subprogram;

   ------------------------------------------------
   -- Generate_Subprogram_Descriptor_For_Package --
   ------------------------------------------------

   procedure Generate_Subprogram_Descriptor_For_Package
     (N    : Node_Id;
      Spec : Entity_Id)
   is
      Adecl : Node_Id;

   begin
      --  If N is empty with prior errors, ignore

      if Total_Errors_Detected /= 0 and then No (N) then
         return;
      end if;

      --  Do not generate if no exceptions

      if Restrictions (No_Exception_Handlers) then
         return;
      end if;

      --  Otherwise generate descriptor

      Adecl := Aux_Decls_Node (Parent (N));

      if No (Actions (Adecl)) then
         Set_Actions (Adecl, New_List);
      end if;

      Generate_Subprogram_Descriptor (N, Sloc (N), Spec, Actions (Adecl));
   end Generate_Subprogram_Descriptor_For_Package;

   ---------------------------------------------------
   -- Generate_Subprogram_Descriptor_For_Subprogram --
   ---------------------------------------------------

   procedure Generate_Subprogram_Descriptor_For_Subprogram
     (N    : Node_Id;
      Spec : Entity_Id)
   is
   begin
      --  If we have no subprogram body and prior errors, ignore

      if Total_Errors_Detected /= 0 and then No (N) then
         return;
      end if;

      --  Do not generate if no exceptions

      if Restrictions (No_Exception_Handlers) then
         return;
      end if;

      --  Else generate descriptor

      declare
         HSS : constant Node_Id := Handled_Statement_Sequence (N);

      begin
         if No (Exception_Handlers (HSS)) then
            Generate_Subprogram_Descriptor
              (N, Sloc (N), Spec, Statements (HSS));
         else
            Generate_Subprogram_Descriptor
              (N, Sloc (N),
               Spec, Statements (Last (Exception_Handlers (HSS))));
         end if;
      end;
   end Generate_Subprogram_Descriptor_For_Subprogram;

   -----------------------------------
   -- Generate_Unit_Exception_Table --
   -----------------------------------

   --  The only remaining thing to generate here is to generate the
   --  reference to the subprogram descriptor chain. See Ada.Exceptions
   --  for details of required data structures.

   procedure Generate_Unit_Exception_Table is
      Loc      : constant Source_Ptr := No_Location;
      Num      : Nat;
      Decl     : Node_Id;
      Ent      : Entity_Id;
      Next_Ent : Entity_Id;
      Stent    : Entity_Id;

   begin
      --  Nothing to be done if zero length exceptions not active

      if Exception_Mechanism /= Front_End_ZCX then
         return;
      end if;

      --  Nothing to do if no exceptions

      if Restrictions (No_Exception_Handlers) then
         return;
      end if;

      --  Remove any entries from SD_List that correspond to eliminated
      --  subprograms.

      Ent := First (SD_List);
      while Present (Ent) loop
         Next_Ent := Next (Ent);
         if Is_Eliminated (Scope (Entity (Prefix (Ent)))) then
            Remove (Ent); -- After this, there is no Next (Ent) anymore
         end if;

         Ent := Next_Ent;
      end loop;

      --  Nothing to do if no unit exception table present.
      --  An empty table can result from subprogram elimination,
      --  in such a case, eliminate the exception table itself.

      if Is_Empty_List (SD_List) then
         Unit_Exception_Table_Present := False;
         return;
      end if;

      --  Do not generate table in a generic

      if Inside_A_Generic then
         return;
      end if;

      --  Generate the unit exception table

      --    subtype Tnn is Subprogram_Descriptors_Record (Num);
      --    __gnat_unitname__SDP : aliased constant Tnn :=
      --                             Num,
      --                             (sub1'unrestricted_access,
      --                              sub2'unrestricted_access,
      --                              ...
      --                              subNum'unrestricted_access));

      Num := List_Length (SD_List);

      Stent :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('T'));

      Insert_Library_Level_Action (
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Stent,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark =>
                New_Occurrence_Of
                 (RTE (RE_Subprogram_Descriptors_Record), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => New_List (
                    Make_Integer_Literal (Loc, Num))))));

      Set_Is_Statically_Allocated (Stent);

      Get_External_Unit_Name_String (Unit_Name (Main_Unit));
      Name_Buffer (1 + 7 .. Name_Len + 7) := Name_Buffer (1 .. Name_Len);
      Name_Buffer (1 .. 7) := "__gnat_";
      Name_Len := Name_Len + 7;
      Add_Str_To_Name_Buffer ("__SDP");

      Ent :=
        Make_Defining_Identifier (Loc,
          Chars => Name_Find);

      Get_Name_String (Chars (Ent));
      Set_Interface_Name (Ent,
        Make_String_Literal (Loc, Strval => String_From_Name_Buffer));

      Decl :=
        Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Object_Definition   => New_Occurrence_Of (Stent, Loc),
          Constant_Present => True,
          Aliased_Present  => True,
          Expression =>
            Make_Aggregate (Loc,
              New_List (
                Make_Integer_Literal (Loc, List_Length (SD_List)),

              Make_Aggregate (Loc,
                Expressions => SD_List))));

      Insert_Library_Level_Action (Decl);

      Set_Is_Exported             (Ent, True);
      Set_Is_Public               (Ent, True);
      Set_Is_Statically_Allocated (Ent, True);

      Get_Name_String (Chars (Ent));
      Set_Interface_Name (Ent,
        Make_String_Literal (Loc,
          Strval => String_From_Name_Buffer));

   end Generate_Unit_Exception_Table;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      SD_List := Empty_List;
   end Initialize;

   ----------------------
   -- Is_Non_Ada_Error --
   ----------------------

   function Is_Non_Ada_Error (E : Entity_Id) return Boolean is
   begin
      if not OpenVMS_On_Target then
         return False;
      end if;

      Get_Name_String (Chars (E));

      --  Note: it is a little irregular for the body of exp_ch11 to know
      --  the details of the encoding scheme for names, but on the other
      --  hand, gigi knows them, and this is for gigi's benefit anyway!

      if Name_Buffer (1 .. 30) /= "system__aux_dec__non_ada_error" then
         return False;
      end if;

      return True;
   end Is_Non_Ada_Error;

   ----------------------------
   -- Remove_Handler_Entries --
   ----------------------------

   procedure Remove_Handler_Entries (N : Node_Id) is
      function Check_Handler_Entry (N : Node_Id) return Traverse_Result;
      --  This function checks one node for a possible reference to a
      --  handler entry that must be deleted. it always returns OK.

      function Remove_All_Handler_Entries is new
        Traverse_Func (Check_Handler_Entry);
      --  This defines the traversal operation

      Discard : Traverse_Result;

      function Check_Handler_Entry (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) = N_Object_Declaration then

            if Present (Handler_List_Entry (N)) then
               Remove (Handler_List_Entry (N));
               Delete_Tree (Handler_List_Entry (N));
               Set_Handler_List_Entry (N, Empty);

            elsif Is_Subprogram_Descriptor (N) then
               declare
                  SDN : Node_Id;

               begin
                  SDN := First (SD_List);
                  while Present (SDN) loop
                     if Defining_Identifier (N) = Entity (Prefix (SDN)) then
                        Remove (SDN);
                        Delete_Tree (SDN);
                        exit;
                     end if;

                     Next (SDN);
                  end loop;
               end;
            end if;
         end if;

         return OK;
      end Check_Handler_Entry;

   --  Start of processing for Remove_Handler_Entries

   begin
      if Exception_Mechanism = Front_End_ZCX then
         Discard := Remove_All_Handler_Entries (N);
      end if;
   end Remove_Handler_Entries;

end Exp_Ch11;
