------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  L I B                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (All_Checks);
--  Subprogram ordering not enforced in this unit
--  (because of some logical groupings).

with Atree;   use Atree;
with Einfo;   use Einfo;
with Fname;   use Fname;
with Namet;   use Namet;
with Namet;   use Namet;
with Output;  use Output;
with Sinfo;   use Sinfo;
with Sinput;  use Sinput;
with Stand;   use Stand;
with Stringt; use Stringt;
with Tree_IO; use Tree_IO;
with Uname;   use Uname;

package body Lib is

   -----------------------
   -- Local Subprograms --
   -----------------------

   type SEU_Result is (
      Yes_Before, -- S1 is in same extended unit as S2 and appears before it
      Yes_Same,   -- S1 is in same extended unit as S2, Slocs are the same
      Yes_After,  -- S1 is in same extended unit as S2, and appears after it
      No);        -- S2 is not in same extended unit as S2

   function Check_Same_Extended_Unit (S1, S2 : Source_Ptr) return SEU_Result;
   --  Used by In_Same_Extended_Unit and Earlier_In_Extended_Unit. Returns
   --  value as described above.

   --------------------------------------------
   -- Access Functions for Unit Table Fields --
   --------------------------------------------

   function Cunit (U : Unit_Number_Type) return Node_Id is
   begin
      return Units.Table (U).Cunit;
   end Cunit;

   function Cunit_Entity (U : Unit_Number_Type) return Entity_Id is
   begin
      return Units.Table (U).Cunit_Entity;
   end Cunit_Entity;

   function Dependency_Num (U : Unit_Number_Type) return Nat is
   begin
      return Units.Table (U).Dependency_Num;
   end Dependency_Num;

   function Dependent_Unit (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Dependent_Unit;
   end Dependent_Unit;

   function Dynamic_Elab (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Dynamic_Elab;
   end Dynamic_Elab;

   function Error_Location (U : Unit_Number_Type) return Source_Ptr is
   begin
      return Units.Table (U).Error_Location;
   end Error_Location;

   function Expected_Unit (U : Unit_Number_Type) return Unit_Name_Type is
   begin
      return Units.Table (U).Expected_Unit;
   end Expected_Unit;

   function Fatal_Error (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Fatal_Error;
   end Fatal_Error;

   function Generate_Code (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Generate_Code;
   end Generate_Code;

   function Has_RACW (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Has_RACW;
   end Has_RACW;

   function Ident_String (U : Unit_Number_Type) return Node_Id is
   begin
      return Units.Table (U).Ident_String;
   end Ident_String;

   function Loading (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Loading;
   end Loading;

   function Main_Priority (U : Unit_Number_Type) return Int is
   begin
      return Units.Table (U).Main_Priority;
   end Main_Priority;

   function Source_Index (U : Unit_Number_Type) return Source_File_Index is
   begin
      return Units.Table (U).Source_Index;
   end Source_Index;

   function Unit_File_Name (U : Unit_Number_Type) return File_Name_Type is
   begin
      return Units.Table (U).Unit_File_Name;
   end Unit_File_Name;

   function Unit_Name (U : Unit_Number_Type) return Unit_Name_Type is
   begin
      return Units.Table (U).Unit_Name;
   end Unit_Name;

   ------------------------------------------
   -- Subprograms to Set Unit Table Fields --
   ------------------------------------------

   procedure Set_Cunit (U : Unit_Number_Type; N : Node_Id) is
   begin
      Units.Table (U).Cunit := N;
   end Set_Cunit;

   procedure Set_Cunit_Entity (U : Unit_Number_Type; E : Entity_Id) is
   begin
      Units.Table (U).Cunit_Entity := E;
      Set_Is_Compilation_Unit (E);
   end Set_Cunit_Entity;

   procedure Set_Dynamic_Elab (U : Unit_Number_Type; B : Boolean := True) is
   begin
      Units.Table (U).Dynamic_Elab := B;
   end Set_Dynamic_Elab;

   procedure Set_Error_Location (U : Unit_Number_Type; W : Source_Ptr) is
   begin
      Units.Table (U).Error_Location := W;
   end Set_Error_Location;

   procedure Set_Fatal_Error (U : Unit_Number_Type; B : Boolean := True) is
   begin
      Units.Table (U).Fatal_Error := B;
   end Set_Fatal_Error;

   procedure Set_Generate_Code (U : Unit_Number_Type; B : Boolean := True) is
   begin
      Units.Table (U).Generate_Code := B;
   end Set_Generate_Code;

   procedure Set_Has_RACW (U : Unit_Number_Type; B : Boolean := True) is
   begin
      Units.Table (U).Has_RACW := B;
   end Set_Has_RACW;

   procedure Set_Ident_String (U : Unit_Number_Type; N : Node_Id) is
   begin
      Units.Table (U).Ident_String := N;
   end Set_Ident_String;

   procedure Set_Loading (U : Unit_Number_Type; B : Boolean := True) is
   begin
      Units.Table (U).Loading := B;
   end Set_Loading;

   procedure Set_Main_Priority (U : Unit_Number_Type; P : Int) is
   begin
      Units.Table (U).Main_Priority := P;
   end Set_Main_Priority;

   procedure Set_Unit_Name (U : Unit_Number_Type; N : Unit_Name_Type) is
   begin
      Units.Table (U).Unit_Name := N;
   end Set_Unit_Name;

   ------------------------------
   -- Check_Same_Extended_Unit --
   ------------------------------

   function Check_Same_Extended_Unit (S1, S2 : Source_Ptr) return SEU_Result is
      Sloc1  : Source_Ptr;
      Sloc2  : Source_Ptr;
      Sind1  : Source_File_Index;
      Sind2  : Source_File_Index;
      Inst1  : Source_Ptr;
      Inst2  : Source_Ptr;
      Unum1  : Unit_Number_Type;
      Unum2  : Unit_Number_Type;
      Unit1  : Node_Id;
      Unit2  : Node_Id;
      Depth1 : Nat;
      Depth2 : Nat;

   begin
      if S1 = No_Location or else S2 = No_Location then
         return No;

      elsif S1 = Standard_Location then
         if S2 = Standard_Location then
            return Yes_Same;
         else
            return No;
         end if;

      elsif S2 = Standard_Location then
         return No;
      end if;

      Sloc1 := S1;
      Sloc2 := S2;
      Unum1 := Get_Code_Unit (Sloc1);
      Unum2 := Get_Code_Unit (Sloc2);

      loop
         Sind1 := Get_Source_File_Index (Sloc1);
         Sind2 := Get_Source_File_Index (Sloc2);

         if Sind1 = Sind2 then
            if Sloc1 < Sloc2 then
               return Yes_Before;
            elsif Sloc1 > Sloc2 then
               return Yes_After;
            else
               return Yes_Same;
            end if;
         end if;

         --  OK, the two nodes are in separate source elements, but this is not
         --  decisive, because of the issue of subunits and instantiations.

         --  First we deal with subunits, since if the subunit is in an
         --  instantiation, we know that the parent is in the corresponding
         --  instantiation, since that is the only way we can have a subunit
         --  that is part of an instantiation.

         Unit1 := Unit (Cunit (Unum1));
         Unit2 := Unit (Cunit (Unum2));

         if Nkind (Unit1) = N_Subunit
           and then Present (Corresponding_Stub (Unit1))
         then
            --  Both in subunits. They could have a common ancestor. If they
            --  do, then the deeper one must have a longer unit name. Replace
            --  the deeper one with its corresponding stub, in order to find
            --  nearest common ancestor, if any.

            if Nkind (Unit2) = N_Subunit
              and then Present (Corresponding_Stub (Unit2))
            then
               if Length_Of_Name (Unit_Name (Unum1)) <
                  Length_Of_Name (Unit_Name (Unum2))
               then
                  Sloc2 := Sloc (Corresponding_Stub (Unit2));
                  Unum2 := Get_Source_Unit (Sloc2);
                  goto Continue;

               else
                  Sloc1 := Sloc (Corresponding_Stub (Unit1));
                  Unum1 := Get_Source_Unit (Sloc1);
                  goto Continue;
               end if;

            --  Nod1 in subunit, Nod2 not

            else
               Sloc1 := Sloc (Corresponding_Stub (Unit1));
               Unum1 := Get_Source_Unit (Sloc1);
               goto Continue;
            end if;

         --  Nod2 in subunit, Nod1 not

         elsif Nkind (Unit2) = N_Subunit
           and then Present (Corresponding_Stub (Unit2))
         then
            Sloc2 := Sloc (Corresponding_Stub (Unit2));
            Unum2 := Get_Source_Unit (Sloc2);
            goto Continue;
         end if;

         --  At this stage we know that neither is a subunit, so we deal
         --  with instantiations, since we culd have a common ancestor

         Inst1 := Instantiation (Sind1);
         Inst2 := Instantiation (Sind2);

         if Inst1 /= No_Location then

            --  Both are instantiations

            if Inst2 /= No_Location then

               Depth1 := Instantiation_Depth (Sloc1);
               Depth2 := Instantiation_Depth (Sloc2);

               if Depth1 < Depth2 then
                  Sloc2 := Inst2;
                  Unum2 := Get_Source_Unit (Sloc2);
                  goto Continue;

               elsif Depth1 > Depth2 then
                  Sloc1 := Inst1;
                  Unum1 := Get_Source_Unit (Sloc1);
                  goto Continue;

               else
                  Sloc1 := Inst1;
                  Sloc2 := Inst2;
                  Unum1 := Get_Source_Unit (Sloc1);
                  Unum2 := Get_Source_Unit (Sloc2);
                  goto Continue;
               end if;

            --  Only first node is in instantiation

            else
               Sloc1 := Inst1;
               Unum1 := Get_Source_Unit (Sloc1);
               goto Continue;
            end if;

         --  Only second node is instantiation

         elsif Inst2 /= No_Location then
            Sloc2 := Inst2;
            Unum2 := Get_Source_Unit (Sloc2);
            goto Continue;
         end if;

         --  No instantiations involved, so we are not in the same unit
         --  However, there is one case still to check, namely the case
         --  where one location is in the spec, and the other in the
         --  corresponding body (the spec location is earlier).

         if Nkind (Unit1) = N_Subprogram_Body
              or else
            Nkind (Unit1) = N_Package_Body
         then
            if Library_Unit (Cunit (Unum1)) = Cunit (Unum2) then
               return Yes_After;
            end if;

         elsif Nkind (Unit2) = N_Subprogram_Body
                 or else
               Nkind (Unit2) = N_Package_Body
         then
            if Library_Unit (Cunit (Unum2)) = Cunit (Unum1) then
               return Yes_Before;
            end if;
         end if;

         --  If that special case does not occur, then we are certain that
         --  the two locations are really in separate units.

         return No;

         <<Continue>>
            null;
      end loop;
   end Check_Same_Extended_Unit;

   -------------------------------
   -- Compilation_Switches_Last --
   -------------------------------

   function Compilation_Switches_Last return Nat is
   begin
      return Compilation_Switches.Last;
   end Compilation_Switches_Last;

   ------------------------------
   -- Earlier_In_Extended_Unit --
   ------------------------------

   function Earlier_In_Extended_Unit (S1, S2 : Source_Ptr) return Boolean is
   begin
      return Check_Same_Extended_Unit (S1, S2) = Yes_Before;
   end Earlier_In_Extended_Unit;

   ----------------------------
   -- Entity_Is_In_Main_Unit --
   ----------------------------

   function Entity_Is_In_Main_Unit (E : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      S := Scope (E);

      while S /= Standard_Standard loop
         if S = Main_Unit_Entity then
            return True;
         elsif Ekind (S) = E_Package and then Is_Child_Unit (S) then
            return False;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end Entity_Is_In_Main_Unit;

   ---------------------------------
   -- Generic_Separately_Compiled --
   ---------------------------------

   function Generic_Separately_Compiled (E : Entity_Id) return Boolean is
   begin
      --  We do not generate object files for internal generics, because
      --  the only thing they would contain is the elaboration boolean, and
      --  we are careful to elaborate all predefined units first anyway, so
      --  this boolean is not needed.

      if Is_Internal_File_Name
          (Fname => Unit_File_Name (Get_Source_Unit (E)),
           Renamings_Included => True)
      then
         return False;

      --  All other generic units do generate object files

      else
         return True;
      end if;
   end Generic_Separately_Compiled;

   function Generic_Separately_Compiled
     (Sfile : File_Name_Type) return Boolean
   is
   begin
      --  Exactly the same as previous function, but works directly on a file
      --  name.

      if Is_Internal_File_Name
          (Fname              => Sfile,
           Renamings_Included => True)
      then
         return False;

      --  All other generic units do generate object files

      else
         return True;
      end if;
   end Generic_Separately_Compiled;

   -------------------
   -- Get_Code_Unit --
   -------------------

   function Get_Code_Unit (S : Source_Ptr) return Unit_Number_Type is
   begin
      --  Search table unless we have No_Location, which can happen if the
      --  relevant location has not been set yet. Happens for example when
      --  we obtain Sloc (Cunit (Main_Unit)) before it is set.

      if S /= No_Location then
         declare
            Source_File : constant Source_File_Index :=
                            Get_Source_File_Index (Top_Level_Location (S));

         begin
            for U in Units.First .. Units.Last loop
               if Source_Index (U) = Source_File then
                  return U;
               end if;
            end loop;
         end;
      end if;

      --  If S was No_Location, or was not in the table, we must be in the
      --  main source unit (and the value has not been placed in the table yet)

      return Main_Unit;
   end Get_Code_Unit;

   function Get_Code_Unit (N : Node_Or_Entity_Id) return Unit_Number_Type is
   begin
      return Get_Code_Unit (Sloc (N));
   end Get_Code_Unit;

   ----------------------------
   -- Get_Compilation_Switch --
   ----------------------------

   function Get_Compilation_Switch (N : Pos) return String_Ptr is
   begin
      if N <= Compilation_Switches.Last then
         return Compilation_Switches.Table (N);

      else
         return null;
      end if;
   end Get_Compilation_Switch;

   ----------------------------------
   -- Get_Cunit_Entity_Unit_Number --
   ----------------------------------

   function Get_Cunit_Entity_Unit_Number
     (E : Entity_Id) return Unit_Number_Type
   is
   begin
      for U in Units.First .. Units.Last loop
         if Cunit_Entity (U) = E then
            return U;
         end if;
      end loop;

      --  If not in the table, must be the main source unit, and we just
      --  have not got it put into the table yet.

      return Main_Unit;
   end Get_Cunit_Entity_Unit_Number;

   ---------------------------
   -- Get_Cunit_Unit_Number --
   ---------------------------

   function Get_Cunit_Unit_Number (N : Node_Id) return Unit_Number_Type is
   begin
      for U in Units.First .. Units.Last loop
         if Cunit (U) = N then
            return U;
         end if;
      end loop;

      --  If not in the table, must be the main source unit, and we just
      --  have not got it put into the table yet.

      return Main_Unit;
   end Get_Cunit_Unit_Number;

   ---------------------
   -- Get_Source_Unit --
   ---------------------

   function Get_Source_Unit (S : Source_Ptr) return Unit_Number_Type is
   begin
      --  Search table unless we have No_Location, which can happen if the
      --  relevant location has not been set yet. Happens for example when
      --  we obtain Sloc (Cunit (Main_Unit)) before it is set.

      if S /= No_Location then
         declare
            Source_File : Source_File_Index :=
                            Get_Source_File_Index (Top_Level_Location (S));

         begin
            Source_File := Get_Source_File_Index (S);
            while Template (Source_File) /= No_Source_File loop
               Source_File := Template (Source_File);
            end loop;

            for U in Units.First .. Units.Last loop
               if Source_Index (U) = Source_File then
                  return U;
               end if;
            end loop;
         end;
      end if;

      --  If S was No_Location, or was not in the table, we must be in the
      --  main source unit (and the value is not got put into the table yet)

      return Main_Unit;
   end Get_Source_Unit;

   function Get_Source_Unit (N : Node_Or_Entity_Id) return Unit_Number_Type is
   begin
      return Get_Source_Unit (Sloc (N));
   end Get_Source_Unit;

   --------------------------------
   -- In_Extended_Main_Code_Unit --
   --------------------------------

   function In_Extended_Main_Code_Unit
     (N : Node_Or_Entity_Id) return Boolean
   is
   begin
      if Sloc (N) = Standard_Location then
         return True;

      elsif Sloc (N) = No_Location then
         return False;

      --  Special case Itypes to test the Sloc of the associated node. The
      --  reason we do this is for possible calls from gigi after -gnatD
      --  processing is complete in sprint. This processing updates the
      --  sloc fields of all nodes in the tree, but itypes are not in the
      --  tree so their slocs do not get updated.

      elsif Nkind (N) = N_Defining_Identifier
        and then Is_Itype (N)
      then
         return In_Extended_Main_Code_Unit (Associated_Node_For_Itype (N));

      --  Otherwise see if we are in the main unit

      elsif Get_Code_Unit (Sloc (N)) = Get_Code_Unit (Cunit (Main_Unit)) then
         return True;

      --  Node may be in spec (or subunit etc) of main unit

      else
         return
           In_Same_Extended_Unit (Sloc (N), Sloc (Cunit (Main_Unit)));
      end if;
   end In_Extended_Main_Code_Unit;

   function In_Extended_Main_Code_Unit (Loc : Source_Ptr) return Boolean is
   begin
      if Loc = Standard_Location then
         return True;

      elsif Loc = No_Location then
         return False;

      --  Otherwise see if we are in the main unit

      elsif Get_Code_Unit (Loc) = Get_Code_Unit (Cunit (Main_Unit)) then
         return True;

      --  Location may be in spec (or subunit etc) of main unit

      else
         return
           In_Same_Extended_Unit (Loc, Sloc (Cunit (Main_Unit)));
      end if;
   end In_Extended_Main_Code_Unit;

   ----------------------------------
   -- In_Extended_Main_Source_Unit --
   ----------------------------------

   function In_Extended_Main_Source_Unit
     (N : Node_Or_Entity_Id) return Boolean
   is
      Nloc : constant Source_Ptr := Sloc (N);
      Mloc : constant Source_Ptr := Sloc (Cunit (Main_Unit));

   begin
      --  If Mloc is not set, it means we are still parsing the main unit,
      --  so everything so far is in the extended main source unit.

      if Mloc = No_Location then
         return True;

      --  Special value cases

      elsif Nloc = Standard_Location then
         return True;

      elsif Nloc = No_Location then
         return False;

      --  Special case Itypes to test the Sloc of the associated node. The
      --  reason we do this is for possible calls from gigi after -gnatD
      --  processing is complete in sprint. This processing updates the
      --  sloc fields of all nodes in the tree, but itypes are not in the
      --  tree so their slocs do not get updated.

      elsif Nkind (N) = N_Defining_Identifier
        and then Is_Itype (N)
      then
         return In_Extended_Main_Source_Unit (Associated_Node_For_Itype (N));

      --  Otherwise compare original locations to see if in same unit

      else
         return
           In_Same_Extended_Unit
             (Original_Location (Nloc), Original_Location (Mloc));
      end if;
   end In_Extended_Main_Source_Unit;

   function In_Extended_Main_Source_Unit
     (Loc : Source_Ptr) return Boolean
   is
      Mloc : constant Source_Ptr := Sloc (Cunit (Main_Unit));

   begin
      --  If Mloc is not set, it means we are still parsing the main unit,
      --  so everything so far is in the extended main source unit.

      if Mloc = No_Location then
         return True;

      --  Special value cases

      elsif Loc = Standard_Location then
         return True;

      elsif Loc = No_Location then
         return False;

      --  Otherwise compare original locations to see if in same unit

      else
         return
           In_Same_Extended_Unit
             (Original_Location (Loc), Original_Location (Mloc));
      end if;
   end In_Extended_Main_Source_Unit;

   -----------------------
   -- In_Same_Code_Unit --
   -----------------------

   function In_Same_Code_Unit (N1, N2 : Node_Or_Entity_Id) return Boolean is
      S1 : constant Source_Ptr := Sloc (N1);
      S2 : constant Source_Ptr := Sloc (N2);

   begin
      if S1 = No_Location or else S2 = No_Location then
         return False;

      elsif S1 = Standard_Location then
         return S2 = Standard_Location;

      elsif S2 = Standard_Location then
         return False;
      end if;

      return Get_Code_Unit (N1) = Get_Code_Unit (N2);
   end In_Same_Code_Unit;

   ---------------------------
   -- In_Same_Extended_Unit --
   ---------------------------

   function In_Same_Extended_Unit (S1, S2 : Source_Ptr) return Boolean is
   begin
      return Check_Same_Extended_Unit (S1, S2) /= No;
   end In_Same_Extended_Unit;

   -------------------------
   -- In_Same_Source_Unit --
   -------------------------

   function In_Same_Source_Unit (N1, N2 : Node_Or_Entity_Id) return Boolean is
      S1 : constant Source_Ptr := Sloc (N1);
      S2 : constant Source_Ptr := Sloc (N2);

   begin
      if S1 = No_Location or else S2 = No_Location then
         return False;

      elsif S1 = Standard_Location then
         return S2 = Standard_Location;

      elsif S2 = Standard_Location then
         return False;
      end if;

      return Get_Source_Unit (N1) = Get_Source_Unit (N2);
   end In_Same_Source_Unit;

   -----------------------------
   -- Increment_Serial_Number --
   -----------------------------

   function Increment_Serial_Number return Nat is
      TSN : Int renames Units.Table (Current_Sem_Unit).Serial_Number;

   begin
      TSN := TSN + 1;
      return TSN;
   end Increment_Serial_Number;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Linker_Option_Lines.Init;
      Load_Stack.Init;
      Units.Init;
      Unit_Exception_Table_Present := False;
      Compilation_Switches.Init;
   end Initialize;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded (Uname : Unit_Name_Type) return Boolean is
   begin
      for Unum in Units.First .. Units.Last loop
         if Uname = Unit_Name (Unum) then
            return True;
         end if;
      end loop;

      return False;
   end Is_Loaded;

   ---------------
   -- Last_Unit --
   ---------------

   function Last_Unit return Unit_Number_Type is
   begin
      return Units.Last;
   end Last_Unit;

   ----------
   -- List --
   ----------

   procedure List (File_Names_Only : Boolean := False) is separate;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Linker_Option_Lines.Locked := True;
      Load_Stack.Locked := True;
      Units.Locked := True;
      Linker_Option_Lines.Release;
      Load_Stack.Release;
      Units.Release;
   end Lock;

   ---------------
   -- Num_Units --
   ---------------

   function Num_Units return Nat is
   begin
      return Int (Units.Last) - Int (Main_Unit) + 1;
   end Num_Units;

   -----------------
   -- Remove_Unit --
   -----------------

   procedure Remove_Unit (U : Unit_Number_Type) is
   begin
      if U = Units.Last then
         Units.Decrement_Last;
      end if;
   end Remove_Unit;

   ----------------------------------
   -- Replace_Linker_Option_String --
   ----------------------------------

   procedure Replace_Linker_Option_String
     (S : String_Id; Match_String : String)
   is
   begin
      if Match_String'Length > 0 then
         for J in 1 .. Linker_Option_Lines.Last loop
            String_To_Name_Buffer (Linker_Option_Lines.Table (J).Option);

            if Match_String = Name_Buffer (1 .. Match_String'Length) then
               Linker_Option_Lines.Table (J).Option := S;
               return;
            end if;
         end loop;
      end if;

      Store_Linker_Option_String (S);
   end Replace_Linker_Option_String;

   ----------
   -- Sort --
   ----------

   procedure Sort (Tbl : in out Unit_Ref_Table) is separate;

   ------------------------------
   -- Store_Compilation_Switch --
   ------------------------------

   procedure Store_Compilation_Switch (Switch : String) is
   begin
      Compilation_Switches.Increment_Last;
      Compilation_Switches.Table (Compilation_Switches.Last) :=
        new String'(Switch);

      --  Fix up --RTS flag which has been transformed by the gcc driver
      --  into -fRTS

      if Switch'Last >= Switch'First + 4
        and then Switch (Switch'First .. Switch'First + 4) = "-fRTS"
      then
         Compilation_Switches.Table
           (Compilation_Switches.Last) (Switch'First + 1) := '-';
      end if;
   end Store_Compilation_Switch;

   --------------------------------
   -- Store_Linker_Option_String --
   --------------------------------

   procedure Store_Linker_Option_String (S : String_Id) is
   begin
      Linker_Option_Lines.Increment_Last;
      Linker_Option_Lines.Table (Linker_Option_Lines.Last) :=
        (Option => S, Unit => Current_Sem_Unit);
   end Store_Linker_Option_String;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
      N : Nat;
      S : String_Ptr;

   begin
      Units.Tree_Read;

      --  Read Compilation_Switches table

      Tree_Read_Int (N);
      Compilation_Switches.Set_Last (N);

      for J in 1 .. N loop
         Tree_Read_Str (S);
         Compilation_Switches.Table (J) := S;
      end loop;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Units.Tree_Write;

      --  Write Compilation_Switches table

      Tree_Write_Int (Compilation_Switches.Last);

      for J in 1 .. Compilation_Switches.Last loop
         Tree_Write_Str (Compilation_Switches.Table (J));
      end loop;
   end Tree_Write;

   -----------------
   -- Version_Get --
   -----------------

   function Version_Get (U : Unit_Number_Type) return Word_Hex_String is
   begin
      return Get_Hex_String (Units.Table (U).Version);
   end Version_Get;

   ------------------------
   -- Version_Referenced --
   ------------------------

   procedure Version_Referenced (S : String_Id) is
   begin
      Version_Ref.Append (S);
   end Version_Referenced;

end Lib;
