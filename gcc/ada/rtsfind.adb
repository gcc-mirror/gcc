------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              R T S F I N D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Dist; use Exp_Dist;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Opt;      use Opt;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Dist; use Sem_Dist;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Uname;    use Uname;

package body Rtsfind is

   RTE_Available_Call : Boolean := False;
   --  Set True during call to RTE from RTE_Available (or from call to
   --  RTE_Record_Component from RTE_Record_Component_Available). Tells
   --  the called subprogram to set RTE_Is_Available to False rather than
   --  generating an error message.

   RTE_Is_Available : Boolean;
   --  Set True by RTE_Available on entry. When RTE_Available_Call is set
   --  True, set False if RTE would otherwise generate an error message.

   ----------------
   -- Unit table --
   ----------------

   --  The unit table has one entry for each unit included in the definition
   --  of the type RTU_Id in the spec. The table entries are initialized in
   --  Initialize to set the Entity field to Empty, indicating that the
   --  corresponding unit has not yet been loaded. The fields are set when
   --  a unit is loaded to contain the defining entity for the unit, the
   --  unit name, and the unit number.

   --  Note that a unit can be loaded either by a call to find an entity
   --  within the unit (e.g. RTE), or by an explicit with of the unit. In
   --  the latter case it is critical to make a call to Set_RTU_Loaded to
   --  ensure that the entry in this table reflects the load.

   --  A unit retrieved through rtsfind  may end up in the context of several
   --  other units, in addition to the main unit. These additional with_clauses
   --  are needed to generate a proper traversal order for Inspector. To
   --  minimize somewhat the redundancy created by numerous calls to rtsfind
   --  from different units, we keep track of the list of implicit with_clauses
   --  already created for the current loaded unit.

   type RT_Unit_Table_Record is record
      Entity               : Entity_Id;
      Uname                : Unit_Name_Type;
      First_Implicit_With  : Node_Id;
      Unum                 : Unit_Number_Type;
   end record;

   RT_Unit_Table : array (RTU_Id) of RT_Unit_Table_Record;

   --------------------------
   -- Runtime Entity Table --
   --------------------------

   --  There is one entry in the runtime entity table for each entity that is
   --  included in the definition of the RE_Id type in the spec. The entries
   --  are set by Initialize_Rtsfind to contain Empty, indicating that the
   --  entity has not yet been located. Once the entity is located for the
   --  first time, its ID is stored in this array, so that subsequent calls
   --  for the same entity can be satisfied immediately.

   --  NOTE: In order to avoid conflicts between record components and subprgs
   --        that have the same name (i.e. subprogram External_Tag and
   --        component External_Tag of package Ada.Tags) this table is not used
   --        with Record_Components.

   RE_Table : array (RE_Id) of Entity_Id;

   --------------------------------
   -- Generation of with_clauses --
   --------------------------------

   --  When a unit is implicitly loaded as a result of a call to RTE, it is
   --  necessary to create one or two implicit with_clauses. We add such
   --  with_clauses to the extended main unit if needed, and also to whatever
   --  unit needs them, which is not necessarily the main unit. The former
   --  ensures that the object is correctly loaded by the binder. The latter
   --  is necessary for SofCheck Inspector.

   --  The field First_Implicit_With in the unit table record are used to
   --  avoid creating duplicate with_clauses.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Check_CRT (E : RE_Id; Eid : Entity_Id) return Entity_Id;
   --  Check entity Eid to ensure that configurable run-time restrictions are
   --  met. May generate an error message (if RTE_Available_Call is false) and
   --  raise RE_Not_Available if entity E does not exist (e.g. Eid is Empty).
   --  Also check that entity is not overloaded.

   procedure Entity_Not_Defined (Id : RE_Id);
   --  Outputs error messages for an entity that is not defined in the run-time
   --  library (the form of the error message is tailored for no run time or
   --  configurable run time mode as required).

   function Get_Unit_Name (U_Id : RTU_Id) return Unit_Name_Type;
   --  Retrieves the Unit Name given a unit id represented by its enumeration
   --  value in RTU_Id.

   procedure Load_Fail (S : String; U_Id : RTU_Id; Id : RE_Id);
   --  Internal procedure called if we can't successfully locate or process a
   --  run-time unit. The parameters give information about the error message
   --  to be given. S is a reason for failing to compile the file and U_Id is
   --  the unit id. RE_Id is the RE_Id originally passed to RTE. The message in
   --  S is one of the following:
   --
   --     "not found"
   --     "had parser errors"
   --     "had semantic errors"
   --
   --  The "not found" case is treated specially in that it is considered
   --  a normal situation in configurable run-time mode, and generates
   --  a warning, but is otherwise ignored.

   procedure Load_RTU
     (U_Id        : RTU_Id;
      Id          : RE_Id   := RE_Null;
      Use_Setting : Boolean := False);
   --  Load the unit whose Id is given if not already loaded. The unit is
   --  loaded and analyzed, and the entry in RT_Unit_Table is updated to
   --  reflect the load. Use_Setting is used to indicate the initial setting
   --  for the Is_Potentially_Use_Visible flag of the entity for the loaded
   --  unit (if it is indeed loaded). A value of False means nothing special
   --  need be done. A value of True indicates that this flag must be set to
   --  True. It is needed only in the Text_IO_Kludge procedure, which may
   --  materialize an entity of Text_IO (or [Wide_]Wide_Text_IO) that was
   --  previously unknown. Id is the RE_Id value of the entity which was
   --  originally requested. Id is used only for error message detail, and if
   --  it is RE_Null, then the attempt to output the entity name is ignored.

   function Make_Unit_Name
     (U : RT_Unit_Table_Record;
      N : Node_Id) return Node_Id;
   --  If the unit is a child unit, build fully qualified name for use in
   --  With_Clause.

   procedure Maybe_Add_With (U : in out RT_Unit_Table_Record);
   --  If necessary, add an implicit with_clause from the current unit to the
   --  one represented by U.

   procedure Output_Entity_Name (Id : RE_Id; Msg : String);
   --  Output continuation error message giving qualified name of entity
   --  corresponding to Id, appending the string given by Msg. This call
   --  is only effective in All_Errors mode.

   function RE_Chars (E : RE_Id) return Name_Id;
   --  Given a RE_Id value returns the Chars of the corresponding entity

   procedure RTE_Error_Msg (Msg : String);
   --  Generates a message by calling Error_Msg_N specifying Current_Error_Node
   --  as the node location using the given Msg text. Special processing in the
   --  case where RTE_Available_Call is set. In this case, no message is output
   --  and instead RTE_Is_Available is set to False. Note that this can only be
   --  used if you are sure that the message comes directly or indirectly from
   --  a call to the RTE function.

   ---------------
   -- Check_CRT --
   ---------------

   function Check_CRT (E : RE_Id; Eid : Entity_Id) return Entity_Id is
      U_Id : constant RTU_Id := RE_Unit_Table (E);

   begin
      if No (Eid) then
         if RTE_Available_Call then
            RTE_Is_Available := False;
         else
            Entity_Not_Defined (E);
         end if;

         raise RE_Not_Available;

      --  Entity is available

      else
         --  If in No_Run_Time mode and entity is not in one of the
         --  specially permitted units, raise the exception.

         if No_Run_Time_Mode
           and then not OK_No_Run_Time_Unit (U_Id)
         then
            Entity_Not_Defined (E);
            raise RE_Not_Available;
         end if;

         --  Check entity is not overloaded, checking for special exceptions

         if Has_Homonym (Eid)
           and then E /= RE_Save_Occurrence
         then
            Set_Standard_Error;
            Write_Str ("Run-time configuration error (");
            Write_Str ("rtsfind entity """);
            Get_Decoded_Name_String (Chars (Eid));
            Set_Casing (Mixed_Case);
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Str (""" is overloaded)");
            Write_Eol;
            raise Unrecoverable_Error;
         end if;

         --  Otherwise entity is accessible

         return Eid;
      end if;
   end Check_CRT;

   ------------------------
   -- Entity_Not_Defined --
   ------------------------

   procedure Entity_Not_Defined (Id : RE_Id) is
   begin
      if No_Run_Time_Mode then

         --  If the error occurs when compiling the body of a predefined
         --  unit for inlining purposes, the body must be illegal in this
         --  mode, and there is no point in continuing.

         if Is_Predefined_File_Name
           (Unit_File_Name (Get_Source_Unit (Sloc (Current_Error_Node))))
         then
            Error_Msg_N
              ("construct not allowed in no run time mode!",
                 Current_Error_Node);
            raise Unrecoverable_Error;

         else
            RTE_Error_Msg ("|construct not allowed in no run time mode");
         end if;

      elsif Configurable_Run_Time_Mode then
         RTE_Error_Msg ("|construct not allowed in this configuration>");
      else
         RTE_Error_Msg ("run-time configuration error");
      end if;

      Output_Entity_Name (Id, "not defined");
   end Entity_Not_Defined;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (U_Id : RTU_Id) return Unit_Name_Type is
      Uname_Chars : constant String := RTU_Id'Image (U_Id);

   begin
      Name_Len := Uname_Chars'Length;
      Name_Buffer (1 .. Name_Len) := Uname_Chars;
      Set_Casing (All_Lower_Case);

      if U_Id in Ada_Child then
         Name_Buffer (4) := '.';

         if U_Id in Ada_Calendar_Child then
            Name_Buffer (13) := '.';

         elsif U_Id in Ada_Dispatching_Child then
            Name_Buffer (16) := '.';

         elsif U_Id in Ada_Interrupts_Child then
            Name_Buffer (15) := '.';

         elsif U_Id in Ada_Real_Time_Child then
            Name_Buffer (14) := '.';

         elsif U_Id in Ada_Streams_Child then
            Name_Buffer (12) := '.';

         elsif U_Id in Ada_Strings_Child then
            Name_Buffer (12) := '.';

         elsif U_Id in Ada_Text_IO_Child then
            Name_Buffer (12) := '.';

         elsif U_Id in Ada_Wide_Text_IO_Child then
            Name_Buffer (17) := '.';

         elsif U_Id in Ada_Wide_Wide_Text_IO_Child then
            Name_Buffer (22) := '.';
         end if;

      elsif U_Id in Interfaces_Child then
         Name_Buffer (11) := '.';

      elsif U_Id in System_Child then
         Name_Buffer (7) := '.';

         if U_Id in System_Multiprocessors_Child then
            Name_Buffer (23) := '.';
         end if;

         if U_Id in System_Storage_Pools_Child then
            Name_Buffer (21) := '.';
         end if;

         if U_Id in System_Strings_Child then
            Name_Buffer (15) := '.';
         end if;

         if U_Id in System_Tasking_Child then
            Name_Buffer (15) := '.';
         end if;

         if U_Id in System_Tasking_Restricted_Child then
            Name_Buffer (26) := '.';
         end if;

         if U_Id in System_Tasking_Protected_Objects_Child then
            Name_Buffer (33) := '.';
         end if;

         if U_Id in System_Tasking_Async_Delays_Child then
            Name_Buffer (28) := '.';
         end if;
      end if;

      --  Add %s at end for spec

      Name_Buffer (Name_Len + 1) := '%';
      Name_Buffer (Name_Len + 2) := 's';
      Name_Len := Name_Len + 2;

      return Name_Find;
   end Get_Unit_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize the unit table

      for J in RTU_Id loop
         RT_Unit_Table (J).Entity := Empty;
      end loop;

      for J in RE_Id loop
         RE_Table (J) := Empty;
      end loop;

      RTE_Is_Available := False;
   end Initialize;

   ------------
   -- Is_RTE --
   ------------

   function Is_RTE (Ent : Entity_Id; E : RE_Id) return Boolean is
      E_Unit_Name   : Unit_Name_Type;
      Ent_Unit_Name : Unit_Name_Type;

      S  : Entity_Id;
      E1 : Entity_Id;
      E2 : Entity_Id;

   begin
      if No (Ent) then
         return False;

      --  If E has already a corresponding entity, check it directly,
      --  going to full views if they exist to deal with the incomplete
      --  and private type cases properly.

      elsif Present (RE_Table (E)) then
         E1 := Ent;

         if Is_Type (E1) and then Present (Full_View (E1)) then
            E1 := Full_View (E1);
         end if;

         E2 := RE_Table (E);

         if Is_Type (E2) and then Present (Full_View (E2)) then
            E2 := Full_View (E2);
         end if;

         return E1 = E2;
      end if;

      --  If the unit containing E is not loaded, we already know that the
      --  entity we have cannot have come from this unit.

      E_Unit_Name := Get_Unit_Name (RE_Unit_Table (E));

      if not Is_Loaded (E_Unit_Name) then
         return False;
      end if;

      --  Here the unit containing the entity is loaded. We have not made
      --  an explicit call to RTE to get the entity in question, but we may
      --  have obtained a reference to it indirectly from some other entity
      --  in the same unit, or some other unit that references it.

      --  Get the defining unit of the entity

      S := Scope (Ent);

      if Ekind (S) /= E_Package then
         return False;
      end if;

      Ent_Unit_Name := Get_Unit_Name (Unit_Declaration_Node (S));

      --  If the defining unit of the entity we are testing is not the
      --  unit containing E, then they cannot possibly match.

      if Ent_Unit_Name /= E_Unit_Name then
         return False;
      end if;

      --  If the units match, then compare the names (remember that no
      --  overloading is permitted in entities fetched using Rtsfind).

      if RE_Chars (E) = Chars (Ent) then
         RE_Table (E) := Ent;

         --  If front-end inlining is enabled, we may be within a body that
         --  contains inlined functions, which has not been retrieved through
         --  rtsfind, and therefore is not yet recorded in the RT_Unit_Table.
         --  Add the unit information now, it must be fully available.

         declare
            U : RT_Unit_Table_Record
                  renames  RT_Unit_Table (RE_Unit_Table (E));
         begin
            if No (U.Entity) then
               U.Entity := S;
               U.Uname  := E_Unit_Name;
               U.Unum   := Get_Source_Unit (S);
            end if;
         end;

         return True;
      else
         return False;
      end if;
   end Is_RTE;

   ------------
   -- Is_RTU --
   ------------

   function Is_RTU (Ent : Entity_Id;  U : RTU_Id) return Boolean is
      E : constant Entity_Id := RT_Unit_Table (U).Entity;
   begin
      return Present (E) and then E = Ent;
   end Is_RTU;

   ----------------------------
   -- Is_Text_IO_Kludge_Unit --
   ----------------------------

   function Is_Text_IO_Kludge_Unit (Nam : Node_Id) return Boolean is
      Prf : Node_Id;
      Sel : Node_Id;

   begin
      if Nkind (Nam) /= N_Expanded_Name then
         return False;
      end if;

      Prf := Prefix (Nam);
      Sel := Selector_Name (Nam);

      if Nkind (Sel) /= N_Expanded_Name
        or else Nkind (Prf) /= N_Identifier
        or else Chars (Prf) /= Name_Ada
      then
         return False;
      end if;

      Prf := Prefix (Sel);
      Sel := Selector_Name (Sel);

      return
        Nkind (Prf) = N_Identifier
          and then
           (Chars (Prf) = Name_Text_IO
              or else
            Chars (Prf) = Name_Wide_Text_IO
              or else
            Chars (Prf) = Name_Wide_Wide_Text_IO)
          and then
        Nkind (Sel) = N_Identifier
          and then
        Chars (Sel) in Text_IO_Package_Name;
   end Is_Text_IO_Kludge_Unit;

   ---------------
   -- Load_Fail --
   ---------------

   procedure Load_Fail (S : String; U_Id : RTU_Id; Id : RE_Id) is
      M : String (1 .. 100);
      P : Natural := 0;

   begin
      --  Output header message

      if Configurable_Run_Time_Mode then
         RTE_Error_Msg ("construct not allowed in configurable run-time mode");
      else
         RTE_Error_Msg ("run-time library configuration error");
      end if;

      --  Output file name and reason string

      M (1 .. 6) := "\file ";
      P := 6;

      Get_Name_String
        (Get_File_Name (RT_Unit_Table (U_Id).Uname, Subunit => False));
      M (P + 1 .. P + Name_Len) := Name_Buffer (1 .. Name_Len);
      P := P + Name_Len;

      M (P + 1) := ' ';
      P := P + 1;

      M (P + 1 .. P + S'Length) := S;
      P := P + S'Length;

      RTE_Error_Msg (M (1 .. P));

      --  Output entity name

      Output_Entity_Name (Id, "not available");

      --  In configurable run time mode, we raise RE_Not_Available, and the
      --  caller is expected to deal gracefully with this. In the case of a
      --  call to RTE_Available, this exception will be caught in Rtsfind,
      --  and result in a returned value of False for the call.

      if Configurable_Run_Time_Mode then
         raise RE_Not_Available;

      --  Here we have a load failure in normal full run time mode. See if we
      --  are in the context of an RTE_Available call. If so, we just raise
      --  RE_Not_Available. This can happen if a unit is unavailable, which
      --  happens for example in the VM case, where the run-time is not
      --  complete, but we do not regard it as a configurable run-time.
      --  If the caller has done an explicit call to RTE_Available, then
      --  clearly the caller is prepared to deal with a result of False.

      elsif RTE_Available_Call then
         RTE_Is_Available := False;
         raise RE_Not_Available;

      --  If we are not in the context of an RTE_Available call, we are really
      --  trying to load an entity that is not there, and that should never
      --  happen, so in this case we signal a fatal error.

      else
         raise Unrecoverable_Error;
      end if;
   end Load_Fail;

   --------------
   -- Load_RTU --
   --------------

   procedure Load_RTU
     (U_Id        : RTU_Id;
      Id          : RE_Id   := RE_Null;
      Use_Setting : Boolean := False)
   is
      U        : RT_Unit_Table_Record renames RT_Unit_Table (U_Id);
      Priv_Par : constant Elist_Id := New_Elmt_List;
      Lib_Unit : Node_Id;

      procedure Save_Private_Visibility;
      --  If the current unit is the body of child unit or the spec of a
      --  private child unit, the private declarations of the parent(s) are
      --  visible. If the unit to be loaded is another public sibling, its
      --  compilation will affect the visibility of the common ancestors.
      --  Indicate those that must be restored.

      procedure Restore_Private_Visibility;
      --  Restore the visibility of ancestors after compiling RTU

      --------------------------------
      -- Restore_Private_Visibility --
      --------------------------------

      procedure Restore_Private_Visibility is
         E_Par : Elmt_Id;

      begin
         E_Par := First_Elmt (Priv_Par);
         while Present (E_Par) loop
            if not In_Private_Part (Node (E_Par)) then
               Install_Private_Declarations (Node (E_Par));
            end if;

            Next_Elmt (E_Par);
         end loop;
      end Restore_Private_Visibility;

      -----------------------------
      -- Save_Private_Visibility --
      -----------------------------

      procedure Save_Private_Visibility is
         Par : Entity_Id;

      begin
         Par := Scope (Current_Scope);
         while Present (Par)
           and then Par /= Standard_Standard
         loop
            if Ekind (Par) = E_Package
              and then Is_Compilation_Unit (Par)
              and then In_Private_Part (Par)
            then
               Append_Elmt (Par, Priv_Par);
            end if;

            Par := Scope (Par);
         end loop;
      end Save_Private_Visibility;

   --  Start of processing for Load_RTU

   begin
      --  Nothing to do if unit is already loaded

      if Present (U.Entity) then
         return;
      end if;

      --  Note if secondary stack is used

      if U_Id = System_Secondary_Stack then
         Opt.Sec_Stack_Used := True;
      end if;

      --  Otherwise we need to load the unit, First build unit name
      --  from the enumeration literal name in type RTU_Id.

      U.Uname                := Get_Unit_Name (U_Id);
      U. First_Implicit_With := Empty;

      --  Now do the load call, note that setting Error_Node to Empty is
      --  a signal to Load_Unit that we will regard a failure to find the
      --  file as a fatal error, and that it should not output any kind
      --  of diagnostics, since we will take care of it here.

      --  We save style checking switches and turn off style checking for
      --  loading the unit, since we don't want any style checking!

      declare
         Save_Style_Check : constant Boolean := Style_Check;
      begin
         Style_Check := False;
         U.Unum :=
           Load_Unit
             (Load_Name  => U.Uname,
              Required   => False,
              Subunit    => False,
              Error_Node => Empty);
         Style_Check := Save_Style_Check;
      end;

      --  Check for bad unit load

      if U.Unum = No_Unit then
         Load_Fail ("not found", U_Id, Id);
      elsif Fatal_Error (U.Unum) then
         Load_Fail ("had parser errors", U_Id, Id);
      end if;

      --  Make sure that the unit is analyzed

      declare
         Was_Analyzed : constant Boolean :=
                          Analyzed (Cunit (Current_Sem_Unit));

      begin
         --  Pretend that the current unit is analyzed, in case it is System
         --  or some such. This allows us to put some declarations, such as
         --  exceptions and packed arrays of Boolean, into System even though
         --  expanding them requires System...

         --  This is a bit odd but works fine. If the RTS unit does not depend
         --  in any way on the current unit, then it never gets back into the
         --  current unit's tree, and the change we make to the current unit
         --  tree is never noticed by anyone (it is undone in a moment). That
         --  is the normal situation.

         --  If the RTS Unit *does* depend on the current unit, for instance,
         --  when you are compiling System, then you had better have finished
         --  analyzing the part of System that is depended on before you try to
         --  load the RTS Unit. This means having the code in System ordered in
         --  an appropriate manner.

         Set_Analyzed (Cunit (Current_Sem_Unit), True);

         if not Analyzed (Cunit (U.Unum)) then

            --  If the unit is already loaded through a limited_with_clause,
            --  the relevant entities must already be available. We do not
            --  want to load and analyze the unit because this would create
            --  a real semantic dependence when the purpose of the limited_with
            --  is precisely to avoid such.

            if From_With_Type (Cunit_Entity (U.Unum)) then
               null;

            else
               Save_Private_Visibility;
               Semantics (Cunit (U.Unum));
               Restore_Private_Visibility;

               if Fatal_Error (U.Unum) then
                  Load_Fail ("had semantic errors", U_Id, Id);
               end if;
            end if;
         end if;

         --  Undo the pretence

         Set_Analyzed (Cunit (Current_Sem_Unit), Was_Analyzed);
      end;

      Lib_Unit := Unit (Cunit (U.Unum));
      U.Entity := Defining_Entity (Lib_Unit);

      if Use_Setting then
         Set_Is_Potentially_Use_Visible (U.Entity, True);
      end if;
   end Load_RTU;

   --------------------
   -- Make_Unit_Name --
   --------------------

   function Make_Unit_Name
     (U : RT_Unit_Table_Record;
      N : Node_Id) return Node_Id is

      Nam  : Node_Id;
      Scop : Entity_Id;

   begin
      Nam  := New_Reference_To (U.Entity, Standard_Location);
      Scop := Scope (U.Entity);

      if Nkind (N) = N_Defining_Program_Unit_Name then
         while Scop /= Standard_Standard loop
            Nam :=
              Make_Expanded_Name (Standard_Location,
                Chars  => Chars (U.Entity),
                Prefix => New_Reference_To (Scop, Standard_Location),
                Selector_Name => Nam);
            Set_Entity (Nam, U.Entity);

            Scop := Scope (Scop);
         end loop;
      end if;

      return Nam;
   end Make_Unit_Name;

   --------------------
   -- Maybe_Add_With --
   --------------------

   procedure Maybe_Add_With (U : in out RT_Unit_Table_Record) is
   begin
      --  We do not need to generate a with_clause for a call issued from
      --  RTE_Component_Available. However, for CodePeer, we need these
      --  additional with's, because for a sequence like "if RTE_Available (X)
      --  then ... RTE (X)" the RTE call fails to create some necessary
      --  with's.

      if RTE_Available_Call and then not Generate_SCIL then
         return;
      end if;

      --  Avoid creating directly self-referential with clauses

      if Current_Sem_Unit = U.Unum then
         return;
      end if;

      --  Add the with_clause, if not already in the context of the
      --  current compilation unit.

      declare
         LibUnit : constant Node_Id := Unit (Cunit (U.Unum));
         Clause  : Node_Id;
         Withn   : Node_Id;

      begin
         Clause := U.First_Implicit_With;
         while Present (Clause) loop
            if Parent (Clause) =  Cunit (Current_Sem_Unit) then
               return;
            end if;

            Clause := Next_Implicit_With (Clause);
         end loop;

         Withn :=
            Make_With_Clause (Standard_Location,
              Name =>
                Make_Unit_Name
                  (U, Defining_Unit_Name (Specification (LibUnit))));

         Set_Library_Unit        (Withn, Cunit (U.Unum));
         Set_Corresponding_Spec  (Withn, U.Entity);
         Set_First_Name          (Withn, True);
         Set_Implicit_With       (Withn, True);
         Set_Next_Implicit_With  (Withn, U.First_Implicit_With);

         U.First_Implicit_With := Withn;

         Mark_Rewrite_Insertion (Withn);
         Append (Withn, Context_Items (Cunit (Current_Sem_Unit)));
         Check_Restriction_No_Dependence (Name (Withn), Current_Error_Node);
      end;
   end Maybe_Add_With;

   ------------------------
   -- Output_Entity_Name --
   ------------------------

   procedure Output_Entity_Name (Id : RE_Id; Msg : String) is
      M : String (1 .. 2048);
      P : Natural := 0;
      --  M (1 .. P) is current message to be output

      RE_Image : constant String := RE_Id'Image (Id);

   begin
      if Id = RE_Null then
         return;
      end if;

      M (1 .. 9) := "\entity """;
      P := 9;

      --  Add unit name to message, excluding %s or %b at end

      Get_Name_String (Get_Unit_Name (RE_Unit_Table (Id)));
      Name_Len := Name_Len - 2;
      Set_Casing (Mixed_Case);
      M (P + 1 .. P + Name_Len) := Name_Buffer (1 .. Name_Len);
      P := P + Name_Len;

      --  Add a qualifying period

      M (P + 1) := '.';
      P := P + 1;

      --  Add entity name and closing quote to message

      Name_Len := RE_Image'Length - 3;
      Name_Buffer (1 .. Name_Len) := RE_Image (4 .. RE_Image'Length);
      Set_Casing (Mixed_Case);
      M (P + 1 .. P + Name_Len) := Name_Buffer (1 .. Name_Len);
      P := P + Name_Len;
      M (P + 1) := '"';
      P := P + 1;

      --  Add message

      M (P + 1) := ' ';
      P := P + 1;
      M (P + 1 .. P + Msg'Length) := Msg;
      P := P + Msg'Length;

      --  Output message at current error node location

      RTE_Error_Msg (M (1 .. P));
   end Output_Entity_Name;

   --------------
   -- RE_Chars --
   --------------

   function RE_Chars (E : RE_Id) return Name_Id is
      RE_Name_Chars : constant String := RE_Id'Image (E);

   begin
      --  Copy name skipping initial RE_ or RO_XX characters

      if RE_Name_Chars (1 .. 2) = "RE" then
         for J in 4 .. RE_Name_Chars'Last loop
            Name_Buffer (J - 3) := Fold_Lower (RE_Name_Chars (J));
         end loop;

         Name_Len := RE_Name_Chars'Length - 3;

      else
         for J in 7 .. RE_Name_Chars'Last loop
            Name_Buffer (J - 6) := Fold_Lower (RE_Name_Chars (J));
         end loop;

         Name_Len := RE_Name_Chars'Length - 6;
      end if;

      return Name_Find;
   end RE_Chars;

   ---------
   -- RTE --
   ---------

   function RTE (E : RE_Id) return Entity_Id is
      U_Id : constant RTU_Id := RE_Unit_Table (E);
      U    : RT_Unit_Table_Record renames RT_Unit_Table (U_Id);

      Lib_Unit : Node_Id;
      Pkg_Ent  : Entity_Id;
      Ename    : Name_Id;

      --  The following flag is used to disable front-end inlining when RTE
      --  is invoked. This prevents the analysis of other runtime bodies when
      --  a particular spec is loaded through Rtsfind. This is both efficient,
      --  and it prevents spurious visibility conflicts between use-visible
      --  user entities, and entities in run-time packages.

      Save_Front_End_Inlining : Boolean;

      procedure Check_RPC;
      --  Reject programs that make use of distribution features not supported
      --  on the current target. Also check that the PCS is compatible with
      --  the code generator version. On such targets (VMS, Vxworks, others?)
      --  we provide a minimal body for System.Rpc that only supplies an
      --  implementation of Partition_Id.

      function Find_Local_Entity (E : RE_Id) return Entity_Id;
      --  This function is used when entity E is in this compilation's main
      --  unit. It gets the value from the already compiled declaration.

      ---------------
      -- Check_RPC --
      ---------------

      procedure Check_RPC is
      begin
         --  Bypass this check if debug flag -gnatdR set

         if Debug_Flag_RR then
            return;
         end if;

         --  Otherwise we need the check if we are going after one of the
         --  critical entities in System.RPC / System.Partition_Interface.

         if E = RE_Do_Rpc
              or else
            E = RE_Do_Apc
              or else
            E = RE_Params_Stream_Type
              or else
            E = RE_Request_Access
         then
            --  If generating RCI stubs, check that we have a real PCS

            if (Distribution_Stub_Mode = Generate_Receiver_Stub_Body
                  or else
                Distribution_Stub_Mode = Generate_Caller_Stub_Body)
              and then Get_PCS_Name = Name_No_DSA
            then
               Set_Standard_Error;
               Write_Str ("distribution feature not supported");
               Write_Eol;
               raise Unrecoverable_Error;

            --  In all cases, check Exp_Dist and System.Partition_Interface
            --  consistency.

            elsif Get_PCS_Version /=
                    Exp_Dist.PCS_Version_Number (Get_PCS_Name)
            then
               Set_Standard_Error;
               Write_Str ("PCS version mismatch: expander ");
               Write_Int (Exp_Dist.PCS_Version_Number (Get_PCS_Name));
               Write_Str (", PCS (");
               Write_Name (Get_PCS_Name);
               Write_Str (") ");
               Write_Int (Get_PCS_Version);
               Write_Eol;
               raise Unrecoverable_Error;
            end if;
         end if;
      end Check_RPC;

      -----------------------
      -- Find_Local_Entity --
      -----------------------

      function Find_Local_Entity (E : RE_Id) return Entity_Id is
         RE_Str : constant String := RE_Id'Image (E);
         Nam    : Name_Id;
         Ent    : Entity_Id;

         Save_Nam : constant String := Name_Buffer (1 .. Name_Len);
         --  Save name buffer and length over call

      begin
         Name_Len := Natural'Max (0, RE_Str'Length - 3);
         Name_Buffer (1 .. Name_Len) :=
           RE_Str (RE_Str'First + 3 .. RE_Str'Last);

         Nam := Name_Find;
         Ent := Entity_Id (Get_Name_Table_Info (Nam));

         Name_Len := Save_Nam'Length;
         Name_Buffer (1 .. Name_Len) := Save_Nam;

         return Ent;
      end Find_Local_Entity;

   --  Start of processing for RTE

   begin
      --  Doing a rtsfind in system.ads is special, as we cannot do this
      --  when compiling System itself. So if we are compiling system then
      --  we should already have acquired and processed the declaration
      --  of the entity. The test is to see if this compilation's main unit
      --  is System. If so, return the value from the already compiled
      --  declaration and otherwise do a regular find.

      --  Not pleasant, but these kinds of annoying recursion when
      --  writing an Ada compiler in Ada have to be broken somewhere!

      if Present (Main_Unit_Entity)
        and then Chars (Main_Unit_Entity) = Name_System
        and then Analyzed (Main_Unit_Entity)
        and then not Is_Child_Unit (Main_Unit_Entity)
      then
         return Check_CRT (E, Find_Local_Entity (E));
      end if;

      Save_Front_End_Inlining := Front_End_Inlining;
      Front_End_Inlining := False;

      --  Load unit if unit not previously loaded

      if No (RE_Table (E)) then
         Load_RTU (U_Id, Id => E);
         Lib_Unit := Unit (Cunit (U.Unum));

         --  In the subprogram case, we are all done, the entity we want
         --  is the entity for the subprogram itself. Note that we do not
         --  bother to check that it is the entity that was requested.
         --  the only way that could fail to be the case is if runtime is
         --  hopelessly misconfigured, and it isn't worth testing for this.

         if Nkind (Lib_Unit) = N_Subprogram_Declaration then
            RE_Table (E) := U.Entity;

         --  Otherwise we must have the package case. First check package
         --  entity itself (e.g. RTE_Name for System.Interrupts.Name)

         else
            pragma Assert (Nkind (Lib_Unit) = N_Package_Declaration);
            Ename := RE_Chars (E);

            --  First we search the package entity chain. If the package
            --  only has a limited view, scan the corresponding list of
            --  incomplete types.

            if From_With_Type (U.Entity) then
               Pkg_Ent := First_Entity (Limited_View (U.Entity));
            else
               Pkg_Ent := First_Entity (U.Entity);
            end if;

            while Present (Pkg_Ent) loop
               if Ename = Chars (Pkg_Ent) then
                  RE_Table (E) := Pkg_Ent;
                  Check_RPC;
                  goto Found;
               end if;

               Next_Entity (Pkg_Ent);
            end loop;

            --  If we did not find the entity in the package entity chain,
            --  then check if the package entity itself matches. Note that
            --  we do this check after searching the entity chain, since
            --  the rule is that in case of ambiguity, we prefer the entity
            --  defined within the package, rather than the package itself.

            if Ename = Chars (U.Entity) then
               RE_Table (E) := U.Entity;
            end if;

            --  If we didn't find the entity we want, something is wrong.
            --  We just leave RE_Table (E) set to Empty and the appropriate
            --  action will be taken by Check_CRT when we exit.

         end if;
      end if;

   <<Found>>
      Maybe_Add_With (U);

      Front_End_Inlining := Save_Front_End_Inlining;
      return Check_CRT (E, RE_Table (E));
   end RTE;

   -------------------
   -- RTE_Available --
   -------------------

   function RTE_Available (E : RE_Id) return Boolean is
      Dummy : Entity_Id;
      pragma Warnings (Off, Dummy);

      Result : Boolean;

      Save_RTE_Available_Call : constant Boolean := RTE_Available_Call;
      Save_RTE_Is_Available   : constant Boolean := RTE_Is_Available;
      --  These are saved recursively because the call to load a unit
      --  caused by an upper level call may perform a recursive call
      --  to this routine during analysis of the corresponding unit.

   begin
      RTE_Available_Call := True;
      RTE_Is_Available := True;
      Dummy := RTE (E);
      Result := RTE_Is_Available;
      RTE_Available_Call := Save_RTE_Available_Call;
      RTE_Is_Available   := Save_RTE_Is_Available;
      return Result;

   exception
      when RE_Not_Available =>
         RTE_Available_Call := Save_RTE_Available_Call;
         RTE_Is_Available   := Save_RTE_Is_Available;
         return False;
   end RTE_Available;

   --------------------------
   -- RTE_Record_Component --
   --------------------------

   function RTE_Record_Component (E : RE_Id) return Entity_Id is
      U_Id     : constant RTU_Id := RE_Unit_Table (E);
      U        : RT_Unit_Table_Record renames RT_Unit_Table (U_Id);
      E1       : Entity_Id;
      Ename    : Name_Id;
      Found_E  : Entity_Id;
      Lib_Unit : Node_Id;
      Pkg_Ent  : Entity_Id;

      --  The following flag is used to disable front-end inlining when
      --  RTE_Record_Component is invoked. This prevents the analysis of other
      --  runtime bodies when a particular spec is loaded through Rtsfind. This
      --  is both efficient, and it prevents spurious visibility conflicts
      --  between use-visible user entities, and entities in run-time packages.

      Save_Front_End_Inlining : Boolean;

   begin
      --  Note: Contrary to subprogram RTE, there is no need to do any special
      --  management with package system.ads because it has no record type
      --  declarations.

      Save_Front_End_Inlining := Front_End_Inlining;
      Front_End_Inlining      := False;

      --  Load unit if unit not previously loaded

      if not Present (U.Entity) then
         Load_RTU (U_Id, Id => E);
      end if;

      Lib_Unit := Unit (Cunit (U.Unum));

      pragma Assert (Nkind (Lib_Unit) = N_Package_Declaration);
      Ename := RE_Chars (E);

      --  Search the entity in the components of record type declarations
      --  found in the package entity chain.

      Found_E := Empty;
      Pkg_Ent := First_Entity (U.Entity);
      Search : while Present (Pkg_Ent) loop
         if Is_Record_Type (Pkg_Ent) then
            E1 := First_Entity (Pkg_Ent);
            while Present (E1) loop
               if Ename = Chars (E1) then
                  pragma Assert (not Present (Found_E));
                  Found_E := E1;
               end if;

               Next_Entity (E1);
            end loop;
         end if;

         Next_Entity (Pkg_Ent);
      end loop Search;

      --  If we didn't find the entity we want, something is wrong. The
      --  appropriate action will be taken by Check_CRT when we exit.

      Maybe_Add_With (U);

      Front_End_Inlining := Save_Front_End_Inlining;
      return Check_CRT (E, Found_E);
   end RTE_Record_Component;

   ------------------------------------
   -- RTE_Record_Component_Available --
   ------------------------------------

   function RTE_Record_Component_Available (E : RE_Id) return Boolean is
      Dummy : Entity_Id;
      pragma Warnings (Off, Dummy);

      Result : Boolean;

      Save_RTE_Available_Call : constant Boolean := RTE_Available_Call;
      Save_RTE_Is_Available   : constant Boolean := RTE_Is_Available;
      --  These are saved recursively because the call to load a unit
      --  caused by an upper level call may perform a recursive call
      --  to this routine during analysis of the corresponding unit.

   begin
      RTE_Available_Call := True;
      RTE_Is_Available := True;
      Dummy := RTE_Record_Component (E);
      Result := RTE_Is_Available;
      RTE_Available_Call := Save_RTE_Available_Call;
      RTE_Is_Available   := Save_RTE_Is_Available;
      return Result;

   exception
      when RE_Not_Available =>
         RTE_Available_Call := Save_RTE_Available_Call;
         RTE_Is_Available   := Save_RTE_Is_Available;
         return False;
   end RTE_Record_Component_Available;

   -------------------
   -- RTE_Error_Msg --
   -------------------

   procedure RTE_Error_Msg (Msg : String) is
   begin
      if RTE_Available_Call then
         RTE_Is_Available := False;
      else
         Error_Msg_N (Msg, Current_Error_Node);

         --  Bump count of violations if we are in configurable run-time
         --  mode and this is not a continuation message.

         if Configurable_Run_Time_Mode and then Msg (Msg'First) /= '\' then
            Configurable_Run_Time_Violations :=
              Configurable_Run_Time_Violations + 1;
         end if;
      end if;
   end RTE_Error_Msg;

   ----------------
   -- RTU_Entity --
   ----------------

   function RTU_Entity (U : RTU_Id) return Entity_Id is
   begin
      return RT_Unit_Table (U).Entity;
   end RTU_Entity;

   ----------------
   -- RTU_Loaded --
   ----------------

   function RTU_Loaded (U : RTU_Id) return Boolean is
   begin
      return Present (RT_Unit_Table (U).Entity);
   end RTU_Loaded;

   --------------------
   -- Set_RTU_Loaded --
   --------------------

   procedure Set_RTU_Loaded (N : Node_Id) is
      Loc   : constant Source_Ptr       := Sloc (N);
      Unum  : constant Unit_Number_Type := Get_Source_Unit (Loc);
      Uname : constant Unit_Name_Type   := Unit_Name (Unum);
      E     : constant Entity_Id        :=
                Defining_Entity (Unit (Cunit (Unum)));
   begin
      pragma Assert (Is_Predefined_File_Name (Unit_File_Name (Unum)));

      --  Loop through entries in RTU table looking for matching entry

      for U_Id in RTU_Id'Range loop

         --  Here we have a match

         if Get_Unit_Name (U_Id) = Uname then
            declare
               U : RT_Unit_Table_Record renames RT_Unit_Table (U_Id);
               --  The RT_Unit_Table entry that may need updating

            begin
               --  If entry is not set, set it now, and indicate that it was
               --  loaded through an explicit context clause.

               if No (U.Entity) then
                  U := (Entity               => E,
                        Uname                => Get_Unit_Name (U_Id),
                        Unum                 => Unum,
                        First_Implicit_With  => Empty);
               end if;

               return;
            end;
         end if;
      end loop;
   end Set_RTU_Loaded;

   --------------------
   -- Text_IO_Kludge --
   --------------------

   procedure Text_IO_Kludge (Nam : Node_Id) is
      Chrs : Name_Id;

      type Name_Map_Type is array (Text_IO_Package_Name) of RTU_Id;

      Name_Map : constant Name_Map_Type := Name_Map_Type'(
        Name_Decimal_IO     => Ada_Text_IO_Decimal_IO,
        Name_Enumeration_IO => Ada_Text_IO_Enumeration_IO,
        Name_Fixed_IO       => Ada_Text_IO_Fixed_IO,
        Name_Float_IO       => Ada_Text_IO_Float_IO,
        Name_Integer_IO     => Ada_Text_IO_Integer_IO,
        Name_Modular_IO     => Ada_Text_IO_Modular_IO);

      Wide_Name_Map : constant Name_Map_Type := Name_Map_Type'(
        Name_Decimal_IO     => Ada_Wide_Text_IO_Decimal_IO,
        Name_Enumeration_IO => Ada_Wide_Text_IO_Enumeration_IO,
        Name_Fixed_IO       => Ada_Wide_Text_IO_Fixed_IO,
        Name_Float_IO       => Ada_Wide_Text_IO_Float_IO,
        Name_Integer_IO     => Ada_Wide_Text_IO_Integer_IO,
        Name_Modular_IO     => Ada_Wide_Text_IO_Modular_IO);

      Wide_Wide_Name_Map : constant Name_Map_Type := Name_Map_Type'(
        Name_Decimal_IO     => Ada_Wide_Wide_Text_IO_Decimal_IO,
        Name_Enumeration_IO => Ada_Wide_Wide_Text_IO_Enumeration_IO,
        Name_Fixed_IO       => Ada_Wide_Wide_Text_IO_Fixed_IO,
        Name_Float_IO       => Ada_Wide_Wide_Text_IO_Float_IO,
        Name_Integer_IO     => Ada_Wide_Wide_Text_IO_Integer_IO,
        Name_Modular_IO     => Ada_Wide_Wide_Text_IO_Modular_IO);

      To_Load : RTU_Id;
      --  Unit to be loaded, from one of the above maps

   begin
      --  Nothing to do if name is not an identifier or a selected component
      --  whose selector_name is an identifier.

      if Nkind (Nam) = N_Identifier then
         Chrs := Chars (Nam);

      elsif Nkind (Nam) = N_Selected_Component
        and then Nkind (Selector_Name (Nam)) = N_Identifier
      then
         Chrs := Chars (Selector_Name (Nam));

      else
         return;
      end if;

      --  Nothing to do if name is not one of the Text_IO subpackages
      --  Otherwise look through loaded units, and if we find Text_IO
      --  or [Wide_]Wide_Text_IO already loaded, then load the proper child.

      if Chrs in Text_IO_Package_Name then
         for U in Main_Unit .. Last_Unit loop
            Get_Name_String (Unit_File_Name (U));

            if Name_Len = 12 then

               --  Here is where we do the loads if we find one of the units
               --  Ada.Text_IO or Ada.[Wide_]Wide_Text_IO. An interesting
               --  detail is that these units may already be used (i.e. their
               --  In_Use flags may be set). Normally when the In_Use flag is
               --  set, the Is_Potentially_Use_Visible flag of all entities in
               --  the package is set, but the new entity we are mysteriously
               --  adding was not there to have its flag set at the time. So
               --  that's why we pass the extra parameter to RTU_Find, to make
               --  sure the flag does get set now. Given that those generic
               --  packages are in fact child units, we must indicate that
               --  they are visible.

               if Name_Buffer (1 .. 12) = "a-textio.ads" then
                  To_Load := Name_Map (Chrs);

               elsif Name_Buffer (1 .. 12) = "a-witeio.ads" then
                  To_Load := Wide_Name_Map (Chrs);

               elsif Name_Buffer (1 .. 12) = "a-ztexio.ads" then
                  To_Load := Wide_Wide_Name_Map (Chrs);

               else
                  goto Continue;
               end if;

               Load_RTU (To_Load, Use_Setting => In_Use (Cunit_Entity (U)));
               Set_Is_Visible_Child_Unit (RT_Unit_Table (To_Load).Entity);

               --  Prevent creation of an implicit 'with' from (for example)
               --  Ada.Wide_Text_IO.Integer_IO to Ada.Text_IO.Integer_IO,
               --  because these could create cycles. First check whether the
               --  simple names match ("integer_io" = "integer_io"), and then
               --  check whether the parent is indeed one of the
               --  [[Wide_]Wide_]Text_IO packages.

               if Chrs = Chars (Cunit_Entity (Current_Sem_Unit)) then
                  declare
                     Parent_Name : constant Unit_Name_Type :=
                                     Get_Parent_Spec_Name
                                       (Unit_Name (Current_Sem_Unit));

                  begin
                     if Parent_Name /= No_Unit_Name then
                        Get_Name_String (Parent_Name);

                        declare
                           P : String renames Name_Buffer (1 .. Name_Len);
                        begin
                           if P = "ada.text_io%s"      or else
                              P = "ada.wide_text_io%s" or else
                              P = "ada.wide_wide_text_io%s"
                           then
                              goto Continue;
                           end if;
                        end;
                     end if;
                  end;
               end if;

               --  Add an implicit with clause from the current unit to the
               --  [[Wide_]Wide_]Text_IO child (if necessary).

               Maybe_Add_With (RT_Unit_Table (To_Load));
            end if;

            <<Continue>> null;
         end loop;
      end if;

   exception
      --  Generate error message if run-time unit not available

      when RE_Not_Available =>
         Error_Msg_N ("& not available", Nam);
   end Text_IO_Kludge;

end Rtsfind;
