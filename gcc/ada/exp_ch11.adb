------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 1                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
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

package body Exp_Ch11 is

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

   --  This expansion is only done if we have front end exception handling.
   --  If we have back end exception handling, then the AT END handler is
   --  left alone, and cleanups (including the exceptional case) are handled
   --  by the back end.

   --  In the front end case, the exception handler described above handles
   --  the exceptional case. The AT END handler is left in the generated tree
   --  and the code generator (e.g. gigi) must still handle proper generation
   --  of cleanup calls for the non-exceptional case.

   procedure Expand_At_End_Handler (HSS : Node_Id; Block : Node_Id) is
      Clean   : constant Entity_Id  := Entity (At_End_Proc (HSS));
      Loc     : constant Source_Ptr := Sloc (Clean);
      Ohandle : Node_Id;
      Stmnts  : List_Id;

   begin
      pragma Assert (Present (Clean));
      pragma Assert (No (Exception_Handlers (HSS)));

      --  Don't expand if back end exception handling active

      if Exception_Mechanism = Back_End_Exceptions then
         return;
      end if;

      --  Don't expand an At End handler if we have already had configurable
      --  run-time violations, since likely this will just be a matter of
      --  generating useless cascaded messages

      if Configurable_Run_Time_Violations > 0 then
         return;
      end if;

      if Restriction_Active (No_Exception_Handlers) then
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
      Handler_Loop : while Present (Handler) loop
         Loc := Sloc (Handler);

         --  Remove source handler if gnat debug flag N is set

         if Debug_Flag_Dot_X and then Comes_From_Source (Handler) then
            declare
               H : constant Node_Id := Handler;
            begin
               Next_Non_Pragma (Handler);
               Remove (H);
               goto Continue_Handler_Loop;
            end;
         end if;

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
               Arg : constant Node_Id :=
                       Make_Function_Call (Loc,
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

      <<Continue_Handler_Loop>>
         null;
      end loop Handler_Loop;

      --  If all handlers got removed by gnatdN, then remove the list

      if Debug_Flag_Dot_X
        and then Is_Empty_List (Exception_Handlers (HSS))
      then
         Set_Exception_Handlers (HSS, No_List);
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
   --                    Name_Length      => exceptE'Length,
   --                    Full_Name        => exceptE'Address,
   --                    HTable_Ptr       => null,
   --                    Import_Code      => 0,
   --                    Raise_Hook       => null,
   --                    );

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
        Make_Character_Literal (Loc,
          Chars              =>  Name_uA,
          Char_Literal_Value =>  UI_From_Int (Character'Pos ('A'))));

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

      --  Raise_Hook component: null

      Append_To (L, Make_Null (Loc));

      Set_Expression (N, Make_Aggregate (Loc, Expressions => L));
      Analyze_And_Resolve (Expression (N), Etype (Id));

      --  Register_Exception (except'Unchecked_Access);

      if not Restriction_Active (No_Exception_Handlers)
        and then not Restriction_Active (No_Exception_Registration)
      then
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
        and then not Restriction_Active (No_Exception_Handlers)
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
      --  If a string expression is present, then the raise statement is
      --  converted to a call:

      --     Raise_Exception (exception-name'Identity, string);

      --  and there is nothing else to do

      if Present (Expression (N)) then
         Rewrite (N,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Raise_Exception), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => Name (N),
                 Attribute_Name => Name_Identity),
               Expression (N))));
         Analyze (N);
         return;
      end if;

      --  Remaining processing is for the case where no string expression
      --  is present.

      --  There is no expansion needed for statement "raise <exception>;" when
      --  compiling for the JVM since the JVM has a built-in exception
      --  mechanism. However we need the keep the expansion for "raise;"
      --  statements. See 4jexcept.ads for details.

      if Present (Name (N)) and then Hostparm.Java_VM then
         return;
      end if;

      --  Don't expand a raise statement that does not come from source
      --  if we have already had configurable run-time violations, since
      --  most likely it will be junk cascaded nonsense.

      if Configurable_Run_Time_Violations > 0
        and then not Comes_From_Source (N)
      then
         return;
      end if;

      --  Convert explicit raise of Program_Error, Constraint_Error, and
      --  Storage_Error into the corresponding raise (in High_Integrity_Mode
      --  all other raises will get normal expansion and be disallowed,
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

            --  Build a C-compatible string in case of no exception handlers,
            --  since this is what the last chance handler is expecting.

            if Restriction_Active (No_Exception_Handlers) then

               --  Generate an empty message if configuration pragma
               --  Suppress_Exception_Locations is set for this unit.

               if Opt.Exception_Locations_Suppressed then
                  Name_Len := 1;
               else
                  Name_Len := Name_Len + 1;
               end if;

               Name_Buffer (Name_Len) := ASCII.NUL;
            end if;

            if Opt.Exception_Locations_Suppressed then
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

end Exp_Ch11;
