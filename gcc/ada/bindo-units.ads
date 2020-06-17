------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          B I N D O . U N I T S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2019-2020, Free Software Foundation, Inc.      --
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

--  For full architecture, see unit Bindo.

--  The following unit contains facilities to collect all elaborable units in
--  the bind and inspect their properties.

with GNAT;      use GNAT;
with GNAT.Sets; use GNAT.Sets;

package Bindo.Units is

   ---------------
   -- Unit sets --
   ---------------

   function Hash_Unit (U_Id : Unit_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Unit);
   --  Obtain the hash value of key U_Id

   package Unit_Sets is new Membership_Sets
     (Element_Type => Unit_Id,
      "="          => "=",
      Hash         => Hash_Unit);

   procedure Collect_Elaborable_Units;
   pragma Inline (Collect_Elaborable_Units);
   --  Gather all units in the bind that require elaboration. The units are
   --  accessible via iterator Elaborable_Units_Iterator.

   function Corresponding_Body (U_Id : Unit_Id) return Unit_Id;
   pragma Inline (Corresponding_Body);
   --  Return the body of a spec unit U_Id

   function Corresponding_Spec (U_Id : Unit_Id) return Unit_Id;
   pragma Inline (Corresponding_Spec);
   --  Return the spec of a body unit U_Id

   function Corresponding_Unit (FNam : File_Name_Type) return Unit_Id;
   pragma Inline (Corresponding_Unit);
   --  Obtain the unit which corresponds to name FNam

   function Corresponding_Unit (UNam : Unit_Name_Type) return Unit_Id;
   pragma Inline (Corresponding_Unit);
   --  Obtain the unit which corresponds to name FNam

   function File_Name (U_Id : Unit_Id) return File_Name_Type;
   pragma Inline (File_Name);
   --  Obtain the file name of unit U_Id

   type Unit_Processor_Ptr is access procedure (U_Id : Unit_Id);

   procedure For_Each_Elaborable_Unit (Processor : Unit_Processor_Ptr);
   pragma Inline (For_Each_Elaborable_Unit);
   --  Invoke Processor on each elaborable unit in the bind

   procedure For_Each_Unit (Processor : Unit_Processor_Ptr);
   pragma Inline (For_Each_Unit);
   --  Invoke Processor on each unit in the bind

   function Has_No_Elaboration_Code (U_Id : Unit_Id) return Boolean;
   pragma Inline (Has_No_Elaboration_Code);
   --  Determine whether unit U_Id lacks elaboration code

   function Hash_Invocation_Signature
     (IS_Id : Invocation_Signature_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Invocation_Signature);
   --  Obtain the hash value of key IS_Id

   function Invocation_Graph_Encoding
     (U_Id : Unit_Id) return Invocation_Graph_Encoding_Kind;
   pragma Inline (Invocation_Graph_Encoding);
   --  Obtain the encoding format used to capture invocation constructs and
   --  relations in the ALI file of unit U_Id.

   function Is_Dynamically_Elaborated (U_Id : Unit_Id) return Boolean;
   pragma Inline (Is_Dynamically_Elaborated);
   --  Determine whether unit U_Id was compiled using the dynamic elaboration
   --  model.

   function Is_Internal_Unit (U_Id : Unit_Id) return Boolean;
   pragma Inline (Is_Internal_Unit);
   --  Determine whether unit U_Id is internal

   function Is_Predefined_Unit (U_Id : Unit_Id) return Boolean;
   pragma Inline (Is_Predefined_Unit);
   --  Determine whether unit U_Id is predefined

   function Name (U_Id : Unit_Id) return Unit_Name_Type;
   pragma Inline (Name);
   --  Obtain the name of unit U_Id

   function Needs_Elaboration (IS_Id : Invocation_Signature_Id) return Boolean;
   pragma Inline (Needs_Elaboration);
   --  Determine whether invocation signature IS_Id belongs to a construct that
   --  appears in a unit which needs to be elaborated.

   function Needs_Elaboration (U_Id : Unit_Id) return Boolean;
   pragma Inline (Needs_Elaboration);
   --  Determine whether unit U_Id needs to be elaborated

   function Number_Of_Elaborable_Units return Natural;
   pragma Inline (Number_Of_Elaborable_Units);
   --  Obtain the number of units in the bind that need to be elaborated

   function Number_Of_Units return Natural;
   pragma Inline (Number_Of_Units);
   --  Obtain the number of units in the bind

   ---------------
   -- Iterators --
   ---------------

   --  The following type represents an iterator over all units that need to be
   --  elaborated.

   type Elaborable_Units_Iterator is private;

   function Has_Next (Iter : Elaborable_Units_Iterator) return Boolean;
   pragma Inline (Has_Next);
   --  Determine whether iterator Iter has more units to examine

   function Iterate_Elaborable_Units return Elaborable_Units_Iterator;
   pragma Inline (Iterate_Elaborable_Units);
   --  Obtain an iterator over all units that need to be elaborated

   procedure Next
     (Iter : in out Elaborable_Units_Iterator;
      U_Id : out Unit_Id);
   pragma Inline (Next);
   --  Return the current unit referenced by iterator Iter and advance to the
   --  next available unit.

   -----------------
   -- Maintenance --
   -----------------

   procedure Finalize_Units;
   pragma Inline (Finalize_Units);
   --  Destroy the internal structures of this unit

   procedure Initialize_Units;
   pragma Inline (Initialize_Units);
   --  Initialize the internal structures of this unit

private
   type Elaborable_Units_Iterator is new Unit_Sets.Iterator;

end Bindo.Units;
