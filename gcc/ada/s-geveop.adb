------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--      S Y S T E M . G E N E R I C _ V E C T O R _ O P E R A T I O N S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2003 Free Software Foundation, Inc.          --
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

with System;                   use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion; use Ada;

package body System.Generic_Vector_Operations is
   VU : constant Address := Vectors.Vector'Size / Storage_Unit;
   EU : constant Address := Element_Array'Component_Size / Storage_Unit;

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

      Unaligned : constant Boolean := (RA or XA or YA)  mod VU /= 0;
      --  False iff one or more argument addresses is not aligned

      type Vector_Ptr is access all Vectors.Vector;
      type Element_Ptr is access all Element;

      function VP is new Unchecked_Conversion (Address, Vector_Ptr);
      function EP is new Unchecked_Conversion (Address, Element_Ptr);

      SA : constant Address := XA + ((Length + 0) / VU * VU
                           and (Boolean'Pos (Unaligned) - Address'(1)));
      --  First address of argument X to start serial processing

   begin
      while XA < SA loop
         VP (RA).all := Vector_Op (VP (XA).all, VP (YA).all);
         XA := XA + VU;
         YA := YA + VU;
         RA := RA + VU;
      end loop;

      while XA < X + Length loop
         EP (RA).all := Element_Op (EP (XA).all, EP (YA).all);
         XA := XA + EU;
         YA := YA + EU;
         RA := RA + EU;
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

      Unaligned : constant Boolean := (RA or XA)  mod VU /= 0;
      --  False iff one or more argument addresses is not aligned

      type Vector_Ptr is access all Vectors.Vector;
      type Element_Ptr is access all Element;

      function VP is new Unchecked_Conversion (Address, Vector_Ptr);
      function EP is new Unchecked_Conversion (Address, Element_Ptr);

      SA : constant Address := XA + ((Length + 0) / VU * VU
                           and (Boolean'Pos (Unaligned) - Address'(1)));
      --  First address of argument X to start serial processing

   begin
      while XA < SA loop
         VP (RA).all := Vector_Op (VP (XA).all);
         XA := XA + VU;
         RA := RA + VU;
      end loop;

      while XA < X + Length loop
         EP (RA).all := Element_Op (EP (XA).all);
         XA := XA + EU;
         RA := RA + EU;
      end loop;
   end Unary_Operation;

end System.Generic_Vector_Operations;
