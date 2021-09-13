------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             G N A T . A L T I V E C . C O N V E R S I O N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2005-2021, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

with System; use System;

package body GNAT.Altivec.Conversions is

   --  All the vector/view conversions operate similarly: bare unchecked
   --  conversion on big endian targets, and elements permutation on little
   --  endian targets. We call "Mirroring" the elements permutation process.

   --  We would like to provide a generic version of the conversion routines
   --  and just have a set of "renaming as body" declarations to satisfy the
   --  public interface. This unfortunately prevents inlining, which we must
   --  preserve at least for the hard binding.

   --  We instead provide a generic version of facilities needed by all the
   --  conversion routines and use them repeatedly.

   generic
      type Vitem_Type is private;

      type Varray_Index_Type is range <>;
      type Varray_Type is array (Varray_Index_Type) of Vitem_Type;

      type Vector_Type is private;
      type View_Type is private;

   package Generic_Conversions is

      subtype Varray is Varray_Type;
      --  This provides an easy common way to refer to the type parameter
      --  in contexts where a specific instance of this package is "use"d.

      procedure Mirror (A : Varray_Type; Into : out Varray_Type);
      pragma Inline (Mirror);
      --  Mirror the elements of A into INTO, not touching the per-element
      --  internal ordering.

      --  A procedure with an out parameter is a bit heavier to use than a
      --  function but reduces the amount of temporary creations around the
      --  call. Instances are typically not front-end inlined. They can still
      --  be back-end inlined on request with the proper command-line option.

      --  Below are Unchecked Conversion routines for various purposes,
      --  relying on internal knowledge about the bits layout in the different
      --  types (all 128 value bits blocks).

      --  View<->Vector straight bitwise conversions on BE targets

      function UNC_To_Vector is
         new Ada.Unchecked_Conversion (View_Type, Vector_Type);

      function UNC_To_View is
         new Ada.Unchecked_Conversion (Vector_Type, View_Type);

      --  Varray->Vector/View for returning mirrored results on LE targets

      function UNC_To_Vector is
         new Ada.Unchecked_Conversion (Varray_Type, Vector_Type);

      function UNC_To_View is
         new Ada.Unchecked_Conversion (Varray_Type, View_Type);

      --  Vector/View->Varray for to-be-permuted source on LE targets

      function UNC_To_Varray is
         new Ada.Unchecked_Conversion (Vector_Type, Varray_Type);

      function UNC_To_Varray is
         new Ada.Unchecked_Conversion (View_Type, Varray_Type);

   end Generic_Conversions;

   package body Generic_Conversions is

      procedure Mirror (A : Varray_Type; Into : out Varray_Type) is
      begin
         for J in A'Range loop
            Into (J) := A (A'Last - J + A'First);
         end loop;
      end Mirror;

   end Generic_Conversions;

   --  Now we declare the instances and implement the interface function
   --  bodies simply calling the instantiated routines.

   ---------------------
   -- Char components --
   ---------------------

   package SC_Conversions is new Generic_Conversions
     (signed_char, Vchar_Range, Varray_signed_char, VSC, VSC_View);

   function To_Vector (S : VSC_View) return VSC is
      use SC_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VSC) return VSC_View is
      use SC_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   --

   package UC_Conversions is new Generic_Conversions
     (unsigned_char, Vchar_Range, Varray_unsigned_char, VUC, VUC_View);

   function To_Vector (S : VUC_View) return VUC is
      use UC_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VUC) return VUC_View is
      use UC_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   --

   package BC_Conversions is new Generic_Conversions
     (bool_char, Vchar_Range, Varray_bool_char, VBC, VBC_View);

   function To_Vector (S : VBC_View) return VBC is
      use BC_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VBC) return VBC_View is
      use BC_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   ----------------------
   -- Short components --
   ----------------------

   package SS_Conversions is new Generic_Conversions
     (signed_short, Vshort_Range, Varray_signed_short, VSS, VSS_View);

   function To_Vector (S : VSS_View) return VSS is
      use SS_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VSS) return VSS_View is
      use SS_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   --

   package US_Conversions is new Generic_Conversions
     (unsigned_short, Vshort_Range, Varray_unsigned_short, VUS, VUS_View);

   function To_Vector (S : VUS_View) return VUS is
      use US_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VUS) return VUS_View is
      use US_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   --

   package BS_Conversions is new Generic_Conversions
     (bool_short, Vshort_Range, Varray_bool_short, VBS, VBS_View);

   function To_Vector (S : VBS_View) return VBS is
      use BS_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VBS) return VBS_View is
      use BS_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   --------------------
   -- Int components --
   --------------------

   package SI_Conversions is new Generic_Conversions
     (signed_int, Vint_Range, Varray_signed_int, VSI, VSI_View);

   function To_Vector (S : VSI_View) return VSI is
      use SI_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VSI) return VSI_View is
      use SI_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   --

   package UI_Conversions is new Generic_Conversions
     (unsigned_int, Vint_Range, Varray_unsigned_int, VUI, VUI_View);

   function To_Vector (S : VUI_View) return VUI is
      use UI_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VUI) return VUI_View is
      use UI_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   --

   package BI_Conversions is new Generic_Conversions
     (bool_int, Vint_Range, Varray_bool_int, VBI, VBI_View);

   function To_Vector (S : VBI_View) return VBI is
      use BI_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VBI) return VBI_View is
      use BI_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   ----------------------
   -- Float components --
   ----------------------

   package F_Conversions is new Generic_Conversions
     (C_float, Vfloat_Range, Varray_float, VF, VF_View);

   function To_Vector (S : VF_View) return VF is
      use F_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VF) return VF_View is
      use F_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

   ----------------------
   -- Pixel components --
   ----------------------

   package P_Conversions is new Generic_Conversions
     (pixel, Vpixel_Range, Varray_pixel, VP, VP_View);

   function To_Vector (S : VP_View) return VP is
      use P_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_Vector (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_Vector (M);
         end;
      end if;
   end To_Vector;

   function To_View (S : VP) return VP_View is
      use P_Conversions;
   begin
      if Default_Bit_Order = High_Order_First then
         return UNC_To_View (S);
      else
         declare
            M : Varray;
         begin
            Mirror (UNC_To_Varray (S), Into => M);
            return UNC_To_View (M);
         end;
      end if;
   end To_View;

end GNAT.Altivec.Conversions;
