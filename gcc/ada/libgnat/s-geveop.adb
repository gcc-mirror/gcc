------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--      S Y S T E M . G E N E R I C _ V E C T O R _ O P E R A T I O N S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Address_Operations; use System.Address_Operations;
with System.Storage_Elements;   use System.Storage_Elements;

with Ada.Unchecked_Conversion;

package body System.Generic_Vector_Operations is

   IU : constant Integer := Integer (Storage_Unit);
   VU : constant Address := Address (Vectors.Vector'Size / IU);
   EU : constant Address := Address (Element_Array'Component_Size / IU);

   ----------------------
   -- Binary_Operation --
   ----------------------

   procedure Binary_Operation
     (R, X, Y : System.Address;
      Length  : System.Storage_Elements.Storage_Count)
   is
      RA : Address := R;
      XA : Address := X;
      YA : Address := Y;
      --  Address of next element to process in R, X and Y

      VI : constant Integer_Address := To_Integer (VU);

      Unaligned : constant Integer_Address :=
                    Boolean'Pos (ModA (OrA (OrA (RA, XA), YA), VU) /= 0) - 1;
      --  Zero iff one or more argument addresses is not aligned, else all 1's

      type Vector_Ptr is access all Vectors.Vector;
      type Element_Ptr is access all Element;

      function VP is new Ada.Unchecked_Conversion (Address, Vector_Ptr);
      function EP is new Ada.Unchecked_Conversion (Address, Element_Ptr);

      pragma Assert (VI > 0);
      --  VI = VU
      --  VU = Vectors.Vector'Size / Storage_Unit
      --  Vector'Size = System.Word_Size
      --  System.Word_Size is a multiple of Storage_Unit
      --  Vector'Size > Storage_Unit
      --  VI > 0
      SA : constant Address :=
             AddA (XA, To_Address
                         ((Integer_Address (Length) / VI * VI) and Unaligned));
      --  First address of argument X to start serial processing

   begin
      while XA < SA loop
         VP (RA).all := Vector_Op (VP (XA).all, VP (YA).all);
         XA := AddA (XA, VU);
         YA := AddA (YA, VU);
         RA := AddA (RA, VU);
      end loop;

      while XA < X + Length loop
         EP (RA).all := Element_Op (EP (XA).all, EP (YA).all);
         XA := AddA (XA, EU);
         YA := AddA (YA, EU);
         RA := AddA (RA, EU);
      end loop;
   end Binary_Operation;

   ----------------------
   -- Unary_Operation --
   ----------------------

   procedure Unary_Operation
     (R, X    : System.Address;
      Length  : System.Storage_Elements.Storage_Count)
   is
      RA : Address := R;
      XA : Address := X;
      --  Address of next element to process in R and X

      VI : constant Integer_Address := To_Integer (VU);

      Unaligned : constant Integer_Address :=
                    Boolean'Pos (ModA (OrA (RA, XA), VU) /= 0) - 1;
      --  Zero iff one or more argument addresses is not aligned, else all 1's

      type Vector_Ptr is access all Vectors.Vector;
      type Element_Ptr is access all Element;

      function VP is new Ada.Unchecked_Conversion (Address, Vector_Ptr);
      function EP is new Ada.Unchecked_Conversion (Address, Element_Ptr);

      pragma Assert (VI > 0);
      --  VI = VU
      --  VU = Vectors.Vector'Size / Storage_Unit
      --  Vector'Size = System.Word_Size
      --  System.Word_Size is a multiple of Storage_Unit
      --  Vector'Size > Storage_Unit
      --  VI > 0
      SA : constant Address :=
             AddA (XA, To_Address
                         ((Integer_Address (Length) / VI * VI) and Unaligned));
      --  First address of argument X to start serial processing

   begin
      while XA < SA loop
         VP (RA).all := Vector_Op (VP (XA).all);
         XA := AddA (XA, VU);
         RA := AddA (RA, VU);
      end loop;

      while XA < X + Length loop
         EP (RA).all := Element_Op (EP (XA).all);
         XA := AddA (XA, EU);
         RA := AddA (RA, EU);
      end loop;
   end Unary_Operation;

end System.Generic_Vector_Operations;
