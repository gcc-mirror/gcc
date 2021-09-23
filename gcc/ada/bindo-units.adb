------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          B I N D O . U N I T S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019-2021, Free Software Foundation, Inc.      --
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

with Bindo.Writers;
use  Bindo.Writers;
use  Bindo.Writers.Phase_Writers;

package body Bindo.Units is

   -------------------
   -- Signature set --
   -------------------

   package Signature_Sets is new Membership_Sets
     (Element_Type => Invocation_Signature_Id,
      "="          => "=",
      Hash         => Hash_Invocation_Signature);

   -----------------
   -- Global data --
   -----------------

   --  The following set stores all invocation signatures that appear in
   --  elaborable units.

   Elaborable_Constructs : Signature_Sets.Membership_Set := Signature_Sets.Nil;

   --  The following set stores all units the need to be elaborated

   Elaborable_Units : Unit_Sets.Membership_Set := Unit_Sets.Nil;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Corresponding_Unit (Nam : Name_Id) return Unit_Id;
   pragma Inline (Corresponding_Unit);
   --  Obtain the unit which corresponds to name Nam

   function Is_Stand_Alone_Library_Unit (U_Id : Unit_Id) return Boolean;
   pragma Inline (Is_Stand_Alone_Library_Unit);
   --  Determine whether unit U_Id is part of a stand-alone library

   procedure Process_Invocation_Construct (IC_Id : Invocation_Construct_Id);
   pragma Inline (Process_Invocation_Construct);
   --  Process invocation construct IC_Id by adding its signature to set
   --  Elaborable_Constructs_Set.

   procedure Process_Invocation_Constructs (U_Id : Unit_Id);
   pragma Inline (Process_Invocation_Constructs);
   --  Process all invocation constructs of unit U_Id for classification
   --  purposes.

   procedure Process_Unit (U_Id : Unit_Id);
   pragma Inline (Process_Unit);
   --  Process unit U_Id for unit classification purposes

   ------------------------------
   -- Collect_Elaborable_Units --
   ------------------------------

   procedure Collect_Elaborable_Units is
   begin
      Start_Phase (Unit_Collection);

      for U_Id in ALI.Units.First .. ALI.Units.Last loop
         Process_Unit (U_Id);
      end loop;

      End_Phase (Unit_Collection);
   end Collect_Elaborable_Units;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   function Corresponding_Body (U_Id : Unit_Id) return Unit_Id is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      pragma Assert (U_Rec.Utype = Is_Spec);
      return U_Id - 1;
   end Corresponding_Body;

   ------------------------
   -- Corresponding_Spec --
   ------------------------

   function Corresponding_Spec (U_Id : Unit_Id) return Unit_Id is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      pragma Assert (U_Rec.Utype = Is_Body);
      return U_Id + 1;
   end Corresponding_Spec;

   ------------------------
   -- Corresponding_Unit --
   ------------------------

   function Corresponding_Unit (FNam : File_Name_Type) return Unit_Id is
   begin
      return Corresponding_Unit (Name_Id (FNam));
   end Corresponding_Unit;

   ------------------------
   -- Corresponding_Unit --
   ------------------------

   function Corresponding_Unit (Nam : Name_Id) return Unit_Id is
   begin
      return Unit_Id (Get_Name_Table_Int (Nam));
   end Corresponding_Unit;

   ------------------------
   -- Corresponding_Unit --
   ------------------------

   function Corresponding_Unit (UNam : Unit_Name_Type) return Unit_Id is
   begin
      return Corresponding_Unit (Name_Id (UNam));
   end Corresponding_Unit;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (U_Id : Unit_Id) return File_Name_Type is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      return U_Rec.Sfile;
   end File_Name;

   --------------------
   -- Finalize_Units --
   --------------------

   procedure Finalize_Units is
   begin
      Signature_Sets.Destroy (Elaborable_Constructs);
      Unit_Sets.Destroy      (Elaborable_Units);
   end Finalize_Units;

   ------------------------------
   -- For_Each_Elaborable_Unit --
   ------------------------------

   procedure For_Each_Elaborable_Unit (Processor : Unit_Processor_Ptr) is
      Iter : Elaborable_Units_Iterator;
      U_Id : Unit_Id;

   begin
      Iter := Iterate_Elaborable_Units;
      while Has_Next (Iter) loop
         Next (Iter, U_Id);

         Processor.all (U_Id);
      end loop;
   end For_Each_Elaborable_Unit;

   -------------------
   -- For_Each_Unit --
   -------------------

   procedure For_Each_Unit (Processor : Unit_Processor_Ptr) is
   begin
      for U_Id in ALI.Units.First .. ALI.Units.Last loop
         Processor.all (U_Id);
      end loop;
   end For_Each_Unit;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (Iter : Elaborable_Units_Iterator) return Boolean is
   begin
      return Unit_Sets.Has_Next (Unit_Sets.Iterator (Iter));
   end Has_Next;

   -----------------------------
   -- Has_No_Elaboration_Code --
   -----------------------------

   function Has_No_Elaboration_Code (U_Id : Unit_Id) return Boolean is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      return U_Rec.No_Elab;
   end Has_No_Elaboration_Code;

   -------------------------------
   -- Hash_Invocation_Signature --
   -------------------------------

   function Hash_Invocation_Signature
     (IS_Id : Invocation_Signature_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (IS_Id));

      return Bucket_Range_Type (IS_Id);
   end Hash_Invocation_Signature;

   ---------------
   -- Hash_Unit --
   ---------------

   function Hash_Unit (U_Id : Unit_Id) return Bucket_Range_Type is
   begin
      pragma Assert (Present (U_Id));

      return Bucket_Range_Type (U_Id);
   end Hash_Unit;

   ----------------------
   -- Initialize_Units --
   ----------------------

   procedure Initialize_Units is
   begin
      Elaborable_Constructs := Signature_Sets.Create (Number_Of_Units);
      Elaborable_Units      := Unit_Sets.Create      (Number_Of_Units);
   end Initialize_Units;

   -------------------------------
   -- Invocation_Graph_Encoding --
   -------------------------------

   function Invocation_Graph_Encoding
     (U_Id : Unit_Id) return Invocation_Graph_Encoding_Kind
   is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);
      U_ALI : ALIs_Record renames ALI.ALIs.Table  (U_Rec.My_ALI);

   begin
      return U_ALI.Invocation_Graph_Encoding;
   end Invocation_Graph_Encoding;

   -------------------------------
   -- Is_Dynamically_Elaborated --
   -------------------------------

   function Is_Dynamically_Elaborated (U_Id : Unit_Id) return Boolean is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      return U_Rec.Dynamic_Elab;
   end Is_Dynamically_Elaborated;

   ----------------------
   -- Is_Internal_Unit --
   ----------------------

   function Is_Internal_Unit (U_Id : Unit_Id) return Boolean is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      return U_Rec.Internal;
   end Is_Internal_Unit;

   ------------------------
   -- Is_Predefined_Unit --
   ------------------------

   function Is_Predefined_Unit (U_Id : Unit_Id) return Boolean is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      return U_Rec.Predefined;
   end Is_Predefined_Unit;

   ---------------------------------
   -- Is_Stand_Alone_Library_Unit --
   ---------------------------------

   function Is_Stand_Alone_Library_Unit (U_Id : Unit_Id) return Boolean is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      return U_Rec.SAL_Interface;
   end Is_Stand_Alone_Library_Unit;

   ------------------------------
   -- Iterate_Elaborable_Units --
   ------------------------------

   function Iterate_Elaborable_Units return Elaborable_Units_Iterator is
   begin
      return Elaborable_Units_Iterator (Unit_Sets.Iterate (Elaborable_Units));
   end Iterate_Elaborable_Units;

   ----------
   -- Name --
   ----------

   function Name (U_Id : Unit_Id) return Unit_Name_Type is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      return U_Rec.Uname;
   end Name;

   -----------------------
   -- Needs_Elaboration --
   -----------------------

   function Needs_Elaboration
     (IS_Id : Invocation_Signature_Id) return Boolean
   is
   begin
      pragma Assert (Present (IS_Id));

      return Signature_Sets.Contains (Elaborable_Constructs, IS_Id);
   end Needs_Elaboration;

   -----------------------
   -- Needs_Elaboration --
   -----------------------

   function Needs_Elaboration (U_Id : Unit_Id) return Boolean is
   begin
      pragma Assert (Present (U_Id));

      return Unit_Sets.Contains (Elaborable_Units, U_Id);
   end Needs_Elaboration;

   ----------
   -- Next --
   ----------

   procedure Next
     (Iter : in out Elaborable_Units_Iterator;
      U_Id : out Unit_Id)
   is
   begin
      Unit_Sets.Next (Unit_Sets.Iterator (Iter), U_Id);
   end Next;

   --------------------------------
   -- Number_Of_Elaborable_Units --
   --------------------------------

   function Number_Of_Elaborable_Units return Natural is
   begin
      return Unit_Sets.Size (Elaborable_Units);
   end Number_Of_Elaborable_Units;

   ---------------------
   -- Number_Of_Units --
   ---------------------

   function Number_Of_Units return Natural is
   begin
      return Natural (ALI.Units.Last) - Natural (ALI.Units.First) + 1;
   end Number_Of_Units;

   ----------------------------------
   -- Process_Invocation_Construct --
   ----------------------------------

   procedure Process_Invocation_Construct (IC_Id : Invocation_Construct_Id) is
      pragma Assert (Present (IC_Id));

      IS_Id : constant Invocation_Signature_Id := Signature (IC_Id);

      pragma Assert (Present (IS_Id));

   begin
      Signature_Sets.Insert (Elaborable_Constructs, IS_Id);
   end Process_Invocation_Construct;

   -----------------------------------
   -- Process_Invocation_Constructs --
   -----------------------------------

   procedure Process_Invocation_Constructs (U_Id : Unit_Id) is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

   begin
      for IC_Id in U_Rec.First_Invocation_Construct ..
                   U_Rec.Last_Invocation_Construct
      loop
         Process_Invocation_Construct (IC_Id);
      end loop;
   end Process_Invocation_Constructs;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (U_Id : Unit_Id) is
   begin
      pragma Assert (Present (U_Id));

      --  A stand-alone library unit must not be elaborated as part of the
      --  current compilation because the library already carries its own
      --  elaboration code.

      if Is_Stand_Alone_Library_Unit (U_Id) then
         null;

      --  Otherwise the unit needs to be elaborated. Add it to the set
      --  of units that require elaboration, as well as all invocation
      --  signatures of constructs it declares.

      else
         Unit_Sets.Insert (Elaborable_Units, U_Id);
         Process_Invocation_Constructs (U_Id);
      end if;
   end Process_Unit;

end Bindo.Units;
