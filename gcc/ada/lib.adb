------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  L I B                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Subprogram ordering not enforced in this unit
--  (because of some logical groupings).

with Atree;          use Atree;
with Csets;          use Csets;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Output;         use Output;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinput;         use Sinput;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Uname;          use Uname;
with Widechar;       use Widechar;

package body Lib is

   Switch_Storing_Enabled : Boolean := True;
   --  Controlled by Enable_Switch_Storing/Disable_Switch_Storing

   -----------------------
   -- Local Subprograms --
   -----------------------

   type SEU_Result is (
      Yes_Before, -- S1 is in same extended unit as S2 and appears before it
      Yes_Same,   -- S1 is in same extended unit as S2, Slocs are the same
      Yes_After,  -- S1 is in same extended unit as S2, and appears after it
      No);        -- S2 is not in same extended unit as S2

   function Check_Same_Extended_Unit
     (S1 : Source_Ptr;
      S2 : Source_Ptr) return SEU_Result;
   --  Used by In_Same_Extended_Unit and Earlier_In_Extended_Unit. Returns
   --  value as described above.

   function Get_Code_Or_Source_Unit
     (S                : Source_Ptr;
      Unwind_Instances : Boolean;
      Unwind_Subunits  : Boolean) return Unit_Number_Type;
   --  Common processing for routines Get_Code_Unit, Get_Source_Unit, and
   --  Get_Top_Level_Code_Unit. Unwind_Instances is True when the unit for the
   --  top-level instantiation should be returned instead of the unit for the
   --  template, in the case of an instantiation. Unwind_Subunits is True when
   --  the corresponding top-level unit should be returned instead of a
   --  subunit, in the case of a subunit.

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

   function Fatal_Error (U : Unit_Number_Type) return Fatal_Type is
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

   function Is_Predefined_Renaming (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Is_Predefined_Renaming;
   end Is_Predefined_Renaming;

   function Is_Internal_Unit       (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Is_Internal_Unit;
   end Is_Internal_Unit;

   function Is_Predefined_Unit     (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Is_Predefined_Unit;
   end Is_Predefined_Unit;

   function Ident_String (U : Unit_Number_Type) return Node_Id is
   begin
      return Units.Table (U).Ident_String;
   end Ident_String;

   function Loading (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Loading;
   end Loading;

   function Main_CPU (U : Unit_Number_Type) return Int is
   begin
      return Units.Table (U).Main_CPU;
   end Main_CPU;

   function Main_Priority (U : Unit_Number_Type) return Int is
   begin
      return Units.Table (U).Main_Priority;
   end Main_Priority;

   function Munit_Index (U : Unit_Number_Type) return Nat is
   begin
      return Units.Table (U).Munit_Index;
   end Munit_Index;

   function No_Elab_Code_All (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).No_Elab_Code_All;
   end No_Elab_Code_All;

   function OA_Setting (U : Unit_Number_Type) return Character is
   begin
      return Units.Table (U).OA_Setting;
   end OA_Setting;

   function Primary_Stack_Count (U : Unit_Number_Type) return Int is
   begin
      return Units.Table (U).Primary_Stack_Count;
   end Primary_Stack_Count;

   function Sec_Stack_Count  (U : Unit_Number_Type) return Int is
   begin
      return Units.Table (U).Sec_Stack_Count;
   end Sec_Stack_Count;

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

   procedure Set_Fatal_Error (U : Unit_Number_Type; V : Fatal_Type) is
   begin
      Units.Table (U).Fatal_Error := V;
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

   procedure Set_Main_CPU (U : Unit_Number_Type; P : Int) is
   begin
      Units.Table (U).Main_CPU := P;
   end Set_Main_CPU;

   procedure Set_Main_Priority (U : Unit_Number_Type; P : Int) is
   begin
      Units.Table (U).Main_Priority := P;
   end Set_Main_Priority;

   procedure Set_No_Elab_Code_All
     (U : Unit_Number_Type;
      B : Boolean := True)
   is
   begin
      Units.Table (U).No_Elab_Code_All := B;
   end Set_No_Elab_Code_All;

   procedure Set_OA_Setting (U : Unit_Number_Type; C : Character) is
   begin
      Units.Table (U).OA_Setting := C;
   end Set_OA_Setting;

   procedure Set_Unit_Name (U : Unit_Number_Type; N : Unit_Name_Type) is
      Old_N : constant Unit_Name_Type := Units.Table (U).Unit_Name;

   begin
      --  First unregister the old name, if any

      if Present (Old_N) and then Unit_Names.Get (Old_N) = U then
         Unit_Names.Set (Old_N, No_Unit);
      end if;

      --  Then set the new name

      Units.Table (U).Unit_Name := N;

      --  Finally register the new name

      if Unit_Names.Get (N) = No_Unit then
         Unit_Names.Set (N, U);
      end if;
   end Set_Unit_Name;

   ------------------------------
   -- Check_Same_Extended_Unit --
   ------------------------------

   function Check_Same_Extended_Unit
     (S1 : Source_Ptr;
      S2 : Source_Ptr) return SEU_Result
   is
      Max_Iterations : constant Nat := Maximum_Instantiations * 2;
      --  Limit to prevent a potential infinite loop

      Counter : Nat := 0;
      Depth1  : Nat;
      Depth2  : Nat;
      Inst1   : Source_Ptr;
      Inst2   : Source_Ptr;
      Sind1   : Source_File_Index;
      Sind2   : Source_File_Index;
      Sloc1   : Source_Ptr;
      Sloc2   : Source_Ptr;
      Unit1   : Node_Id;
      Unit2   : Node_Id;
      Unum1   : Unit_Number_Type;
      Unum2   : Unit_Number_Type;

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

      Unum1 := Get_Source_Unit (Sloc1);
      Unum2 := Get_Source_Unit (Sloc2);

      loop
         --  Step 1: Check whether the two locations are in the same source
         --  file.

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

         --  Step 2: Check subunits. If a subunit is instantiated, follow the
         --  instantiation chain rather than the stub chain.

         --  Note that we must handle the case where the subunit exists in the
         --  same body as the main unit (which may happen when Naming gets
         --  manually specified within a project file or through tools like
         --  gprname). Otherwise, we will have an infinite loop jumping around
         --  the same file.

         Unit1 := Unit (Cunit (Unum1));
         Unit2 := Unit (Cunit (Unum2));
         Inst1 := Instantiation (Sind1);
         Inst2 := Instantiation (Sind2);

         if Nkind (Unit1) = N_Subunit
           and then Present (Corresponding_Stub (Unit1))
           and then Inst1 = No_Location
         then
            if Nkind (Unit2) = N_Subunit
              and then Present (Corresponding_Stub (Unit2))
              and then Inst2 = No_Location
            then
               --  Both locations refer to subunits which may have a common
               --  ancestor. If they do, the deeper subunit must have a longer
               --  unit name. Replace the deeper one with its corresponding
               --  stub in order to find the nearest ancestor.

               if Length_Of_Name (Unit_Name (Unum1)) <
                  Length_Of_Name (Unit_Name (Unum2))
               then
                  Sloc2 := Sloc (Corresponding_Stub (Unit2));

                  if Unum2 /= Get_Source_Unit (Sloc2) then
                     Unum2 := Get_Source_Unit (Sloc2);
                     goto Continue;
                  else
                     null; --  Unum2 already designates the correct unit
                  end if;
               else
                  Sloc1 := Sloc (Corresponding_Stub (Unit1));

                  if Unum1 /= Get_Source_Unit (Sloc1) then
                     Unum1 := Get_Source_Unit (Sloc1);
                     goto Continue;
                  else
                     null; --  Unum1 already designates the correct unit
                  end if;
               end if;

            --  Sloc1 in subunit, Sloc2 not

            else
               Sloc1 := Sloc (Corresponding_Stub (Unit1));

               if Unum1 /= Get_Source_Unit (Sloc1) then
                  Unum1 := Get_Source_Unit (Sloc1);
                  goto Continue;
               else
                  null; --  Unum1 already designates the correct unit
               end if;
            end if;

         --  Sloc2 in subunit, Sloc1 not

         elsif Nkind (Unit2) = N_Subunit
           and then Present (Corresponding_Stub (Unit2))
           and then Inst2 = No_Location
         then
            Sloc2 := Sloc (Corresponding_Stub (Unit2));

            if Unum2 /= Get_Source_Unit (Sloc2) then
               Unum2 := Get_Source_Unit (Sloc2);
               goto Continue;
            else
               null; --  Unum2 already designates the correct unit
            end if;
         end if;

         --  Step 3: Check instances. The two locations may yield a common
         --  ancestor.

         if Inst1 /= No_Location then
            if Inst2 /= No_Location then

               --  Both locations denote instantiations

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

            --  Sloc1 is an instantiation

            else
               Sloc1 := Inst1;
               Unum1 := Get_Source_Unit (Sloc1);
               goto Continue;
            end if;

         --  Sloc2 is an instantiation

         elsif Inst2 /= No_Location then
            Sloc2 := Inst2;
            Unum2 := Get_Source_Unit (Sloc2);
            goto Continue;
         end if;

         --  Step 4: One location in the spec, the other in the corresponding
         --  body of the same unit. The location in the spec is considered
         --  earlier.

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

         --  At this point it is certain that the two locations denote two
         --  entirely separate units.

         return No;

         <<Continue>>
         Counter := Counter + 1;

         --  Prevent looping forever

         if Counter > Max_Iterations then

            --  In CodePeer_Mode, return a value to be able to generate SCIL
            --  files and hope for the best.

            if CodePeer_Mode then
               return No;
            else
               raise Program_Error;
            end if;
         end if;
      end loop;
   end Check_Same_Extended_Unit;

   -------------------------------
   -- Compilation_Switches_Last --
   -------------------------------

   function Compilation_Switches_Last return Nat is
   begin
      return Compilation_Switches.Last;
   end Compilation_Switches_Last;

   ---------------------------
   -- Enable_Switch_Storing --
   ---------------------------

   procedure Enable_Switch_Storing is
   begin
      Switch_Storing_Enabled := True;
   end Enable_Switch_Storing;

   ----------------------------
   -- Disable_Switch_Storing --
   ----------------------------

   procedure Disable_Switch_Storing is
   begin
      Switch_Storing_Enabled := False;
   end Disable_Switch_Storing;

   ------------------------------
   -- Earlier_In_Extended_Unit --
   ------------------------------

   function Earlier_In_Extended_Unit
     (S1 : Source_Ptr;
      S2 : Source_Ptr) return Boolean
   is
   begin
      return Check_Same_Extended_Unit (S1, S2) = Yes_Before;
   end Earlier_In_Extended_Unit;

   function Earlier_In_Extended_Unit
     (N1 : Node_Or_Entity_Id;
      N2 : Node_Or_Entity_Id) return Boolean
   is
   begin
      return Earlier_In_Extended_Unit (Sloc (N1), Sloc (N2));
   end Earlier_In_Extended_Unit;

   -----------------------
   -- Exact_Source_Name --
   -----------------------

   function Exact_Source_Name (Loc : Source_Ptr) return String is
      U    : constant Unit_Number_Type  := Get_Source_Unit (Loc);
      Buf  : constant Source_Buffer_Ptr := Source_Text (Source_Index (U));
      Orig : constant Source_Ptr        := Original_Location (Loc);
      P    : Source_Ptr;

      WC   : Char_Code;
      Err  : Boolean;
      pragma Warnings (Off, WC);
      pragma Warnings (Off, Err);

   begin
      --  Entity is character literal

      if Buf (Orig) = ''' then
         return String (Buf (Orig .. Orig + 2));

      --  Entity is operator symbol

      elsif Buf (Orig) = '"' or else Buf (Orig) = '%' then
         P := Orig;

         loop
            P := P + 1;
            exit when Buf (P) = Buf (Orig);
         end loop;

         return String (Buf (Orig .. P));

      --  Entity is identifier

      else
         P := Orig;

         loop
            if Is_Start_Of_Wide_Char (Buf, P) then
               Scan_Wide (Buf, P, WC, Err);
            elsif not Identifier_Char (Buf (P)) then
               exit;
            else
               P := P + 1;
            end if;
         end loop;

         --  Write out the identifier by copying the exact source characters
         --  used in its declaration. Note that this means wide characters will
         --  be in their original encoded form.

         return String (Buf (Orig .. P - 1));
      end if;
   end Exact_Source_Name;

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

   --------------------------
   -- Generic_May_Lack_ALI --
   --------------------------

   function Generic_May_Lack_ALI (Unum : Unit_Number_Type) return Boolean is
   begin
      --  We allow internal generic units to be used without having a
      --  corresponding ALI files to help bootstrapping with older compilers
      --  that did not support generating ALIs for such generics. It is safe
      --  to do so because the only thing the generated code would contain
      --  is the elaboration boolean, and we are careful to elaborate all
      --  predefined units first anyway.

      return Is_Internal_Unit (Unum);
   end Generic_May_Lack_ALI;

   -----------------------------
   -- Get_Code_Or_Source_Unit --
   -----------------------------

   function Get_Code_Or_Source_Unit
     (S                : Source_Ptr;
      Unwind_Instances : Boolean;
      Unwind_Subunits  : Boolean) return Unit_Number_Type
   is
   begin
      --  Search table unless we have No_Location, which can happen if the
      --  relevant location has not been set yet. Happens for example when
      --  we obtain Sloc (Cunit (Main_Unit)) before it is set.

      if S /= No_Location then
         declare
            Source_File : Source_File_Index;
            Source_Unit : Unit_Number_Type;
            Unit_Node   : Node_Id;

         begin
            Source_File := Get_Source_File_Index (S);

            if Unwind_Instances then
               while Template (Source_File) > No_Source_File loop
                  Source_File := Template (Source_File);
               end loop;
            end if;

            Source_Unit := Unit (Source_File);

            if Unwind_Subunits then
               Unit_Node := Unit (Cunit (Source_Unit));

               while Nkind (Unit_Node) = N_Subunit
                 and then Present (Corresponding_Stub (Unit_Node))
               loop
                  Source_Unit :=
                    Get_Code_Or_Source_Unit
                      (Sloc (Corresponding_Stub (Unit_Node)),
                       Unwind_Instances => Unwind_Instances,
                       Unwind_Subunits  => Unwind_Subunits);
                  Unit_Node := Unit (Cunit (Source_Unit));
               end loop;
            end if;

            if Source_Unit /= No_Unit then
               return Source_Unit;
            end if;
         end;
      end if;

      --  If S was No_Location, or was not in the table, we must be in the main
      --  source unit (and the value has not been placed in the table yet),
      --  or in one of the configuration pragma files.

      return Main_Unit;
   end Get_Code_Or_Source_Unit;

   -------------------
   -- Get_Code_Unit --
   -------------------

   function Get_Code_Unit (S : Source_Ptr) return Unit_Number_Type is
   begin
      return
        Get_Code_Or_Source_Unit
          (Top_Level_Location (S),
           Unwind_Instances => False,
           Unwind_Subunits  => False);
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

      --  If not in the table, must be a spec created for a main unit that is a
      --  child subprogram body which we have not inserted into the table yet.

      if N = Library_Unit (Cunit (Main_Unit)) then
         return Main_Unit;

      --  If it is anything else, something is seriously wrong, and we really
      --  don't want to proceed, even if assertions are off, so we explicitly
      --  raise an exception in this case to terminate compilation.

      else
         raise Program_Error;
      end if;
   end Get_Cunit_Unit_Number;

   ---------------------
   -- Get_Source_Unit --
   ---------------------

   function Get_Source_Unit (S : Source_Ptr) return Unit_Number_Type is
   begin
      return
        Get_Code_Or_Source_Unit
          (S                => S,
           Unwind_Instances => True,
           Unwind_Subunits  => False);
   end Get_Source_Unit;

   function Get_Source_Unit (N : Node_Or_Entity_Id) return Unit_Number_Type is
   begin
      return Get_Source_Unit (Sloc (N));
   end Get_Source_Unit;

   -----------------------------
   -- Get_Top_Level_Code_Unit --
   -----------------------------

   function Get_Top_Level_Code_Unit (S : Source_Ptr) return Unit_Number_Type is
   begin
      return
        Get_Code_Or_Source_Unit
          (Top_Level_Location (S),
           Unwind_Instances => False,
           Unwind_Subunits  => True);
   end Get_Top_Level_Code_Unit;

   function Get_Top_Level_Code_Unit
     (N : Node_Or_Entity_Id) return Unit_Number_Type is
   begin
      return Get_Top_Level_Code_Unit (Sloc (N));
   end Get_Top_Level_Code_Unit;

   --------------------------------
   -- In_Extended_Main_Code_Unit --
   --------------------------------

   function In_Extended_Main_Code_Unit
     (N : Node_Or_Entity_Id) return Boolean
   is
   begin
      if Sloc (N) = Standard_Location then
         return False;

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
         return In_Same_Extended_Unit (N, Cunit (Main_Unit));
      end if;
   end In_Extended_Main_Code_Unit;

   function In_Extended_Main_Code_Unit (Loc : Source_Ptr) return Boolean is
   begin
      if Loc = Standard_Location then
         return False;

      elsif Loc = No_Location then
         return False;

      --  Otherwise see if we are in the main unit

      elsif Get_Code_Unit (Loc) = Get_Code_Unit (Cunit (Main_Unit)) then
         return True;

      --  Location may be in spec (or subunit etc) of main unit

      else
         return In_Same_Extended_Unit (Loc, Sloc (Cunit (Main_Unit)));
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
      --  If parsing, then use the global flag to indicate result

      if Compiler_State = Parsing then
         return Parsing_Main_Extended_Source;

      --  Special value cases

      elsif Nloc = Standard_Location then
         return False;

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
      --  If parsing, then use the global flag to indicate result

      if Compiler_State = Parsing then
         return Parsing_Main_Extended_Source;

      --  Special value cases

      elsif Loc = Standard_Location then
         return False;

      elsif Loc = No_Location then
         return False;

      --  Otherwise compare original locations to see if in same unit

      else
         return
           In_Same_Extended_Unit
             (Original_Location (Loc), Original_Location (Mloc));
      end if;
   end In_Extended_Main_Source_Unit;

   ----------------------
   -- In_Internal_Unit --
   ----------------------

   function In_Internal_Unit (N : Node_Or_Entity_Id) return Boolean is
   begin
      return In_Internal_Unit (Sloc (N));
   end In_Internal_Unit;

   function In_Internal_Unit (S : Source_Ptr) return Boolean is
      Unit : constant Unit_Number_Type := Get_Source_Unit (S);
   begin
      return Is_Internal_Unit (Unit);
   end In_Internal_Unit;

   ----------------------------
   -- In_Predefined_Renaming --
   ----------------------------

   function In_Predefined_Renaming (N : Node_Or_Entity_Id) return Boolean is
   begin
      return In_Predefined_Renaming (Sloc (N));
   end In_Predefined_Renaming;

   function In_Predefined_Renaming (S : Source_Ptr) return Boolean is
      Unit : constant Unit_Number_Type := Get_Source_Unit (S);
   begin
      return Is_Predefined_Renaming (Unit);
   end In_Predefined_Renaming;

   ------------------------
   -- In_Predefined_Unit --
   ------------------------

   function In_Predefined_Unit (N : Node_Or_Entity_Id) return Boolean is
   begin
      return In_Predefined_Unit (Sloc (N));
   end In_Predefined_Unit;

   function In_Predefined_Unit (S : Source_Ptr) return Boolean is
      Unit : constant Unit_Number_Type := Get_Source_Unit (S);
   begin
      return Is_Predefined_Unit (Unit);
   end In_Predefined_Unit;

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

   function In_Same_Extended_Unit
     (N1, N2 : Node_Or_Entity_Id) return Boolean
   is
   begin
      return Check_Same_Extended_Unit (Sloc (N1), Sloc (N2)) /= No;
   end In_Same_Extended_Unit;

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

   -----------------------------------
   -- Increment_Primary_Stack_Count --
   -----------------------------------

   procedure Increment_Primary_Stack_Count (Increment : Int) is
      PSC : Int renames Units.Table (Current_Sem_Unit).Primary_Stack_Count;
   begin
      PSC := PSC + Increment;
   end Increment_Primary_Stack_Count;

   -------------------------------
   -- Increment_Sec_Stack_Count --
   -------------------------------

   procedure Increment_Sec_Stack_Count (Increment : Int) is
      SSC : Int renames Units.Table (Current_Sem_Unit).Sec_Stack_Count;
   begin
      SSC := SSC + Increment;
   end Increment_Sec_Stack_Count;

   -----------------------------
   -- Increment_Serial_Number --
   -----------------------------

   function Increment_Serial_Number return Nat is
      TSN : Int renames Units.Table (Current_Sem_Unit).Serial_Number;
   begin
      TSN := TSN + 1;
      return TSN;
   end Increment_Serial_Number;

   ----------------------
   --  Init_Unit_Name  --
   ----------------------

   procedure Init_Unit_Name (U : Unit_Number_Type; N : Unit_Name_Type) is
   begin
      Units.Table (U).Unit_Name := N;
      Unit_Names.Set (N, U);
   end Init_Unit_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Linker_Option_Lines.Init;
      Notes.Init;
      Load_Stack.Init;
      Units.Init;
      Compilation_Switches.Init;
   end Initialize;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded (Uname : Unit_Name_Type) return Boolean is
   begin
      return Unit_Names.Get (Uname) /= No_Unit;
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
      Linker_Option_Lines.Release;
      Linker_Option_Lines.Locked := True;
      Load_Stack.Release;
      Load_Stack.Locked := True;
      Units.Release;
      Units.Locked := True;
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
         Unit_Names.Set (Unit_Name (U), No_Unit);
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
      if Switch_Storing_Enabled then
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
      end if;
   end Store_Compilation_Switch;

   --------------------------------
   -- Store_Linker_Option_String --
   --------------------------------

   procedure Store_Linker_Option_String (S : String_Id) is
   begin
      Linker_Option_Lines.Append ((Option => S, Unit => Current_Sem_Unit));
   end Store_Linker_Option_String;

   ----------------
   -- Store_Note --
   ----------------

   procedure Store_Note (N : Node_Id) is
      Sfile : constant Source_File_Index := Get_Source_File_Index (Sloc (N));

   begin
      --  Notes for a generic are emitted when processing the template, never
      --  in instances.

      if In_Extended_Main_Code_Unit (N)
        and then Instance (Sfile) = No_Instance_Id
      then
         Notes.Append (N);
      end if;
   end Store_Note;

   -------------------------------
   -- Synchronize_Serial_Number --
   -------------------------------

   procedure Synchronize_Serial_Number (SN : Nat) is
      TSN : Int renames Units.Table (Current_Sem_Unit).Serial_Number;
   begin
      --  We should not be trying to synchronize downward

      pragma Assert (TSN <= SN);

      if TSN < SN then
         TSN := SN;
      end if;
   end Synchronize_Serial_Number;

   --------------------
   -- Unit_Name_Hash --
   --------------------

   function Unit_Name_Hash (Id : Unit_Name_Type) return Unit_Name_Header_Num is
   begin
      return Unit_Name_Header_Num (Id mod Unit_Name_Table_Size);
   end Unit_Name_Hash;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Linker_Option_Lines.Locked := False;
      Load_Stack.Locked := False;
      Units.Locked := False;
   end Unlock;

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

   ---------------------
   -- Write_Unit_Info --
   ---------------------

   procedure Write_Unit_Info
     (Unit_Num : Unit_Number_Type;
      Item     : Node_Id;
      Prefix   : String := "";
      Withs    : Boolean := False)
   is
   begin
      Write_Str (Prefix);
      Write_Unit_Name (Unit_Name (Unit_Num));
      Write_Str (", unit ");
      Write_Int (Int (Unit_Num));
      Write_Str (", ");
      Write_Int (Int (Item));
      Write_Str ("=");
      Write_Str (Node_Kind'Image (Nkind (Item)));

      if Is_Rewrite_Substitution (Item) then
         Write_Str (", orig = ");
         Write_Int (Int (Original_Node (Item)));
         Write_Str ("=");
         Write_Str (Node_Kind'Image (Nkind (Original_Node (Item))));
      end if;

      Write_Eol;

      --  Skip the rest if we're not supposed to print the withs

      if not Withs then
         return;
      end if;

      declare
         Context_Item : Node_Id;

      begin
         Context_Item := First (Context_Items (Cunit (Unit_Num)));
         while Present (Context_Item)
           and then (Nkind (Context_Item) /= N_With_Clause
                      or else Limited_Present (Context_Item))
         loop
            Next (Context_Item);
         end loop;

         if Present (Context_Item) then
            Indent;
            Write_Line ("withs:");
            Indent;

            while Present (Context_Item) loop
               if Nkind (Context_Item) = N_With_Clause
                 and then not Limited_Present (Context_Item)
               then
                  pragma Assert (Present (Library_Unit (Context_Item)));
                  Write_Unit_Name
                    (Unit_Name
                       (Get_Cunit_Unit_Number (Library_Unit (Context_Item))));

                  if Implicit_With (Context_Item) then
                     Write_Str (" -- implicit");
                  end if;

                  Write_Eol;
               end if;

               Next (Context_Item);
            end loop;

            Outdent;
            Write_Line ("end withs");
            Outdent;
         end if;
      end;
   end Write_Unit_Info;

end Lib;
