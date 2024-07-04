------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . G E N E R I C _ A R R A Y _ S O R T     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2024, Free Software Foundation, Inc.         --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

with Ada.Containers.Generic_Constrained_Array_Sort;

procedure Ada.Containers.Generic_Array_Sort
  (Container : in out Array_Type)
is
   subtype Index_Subtype is
     Index_Type range Container'First .. Container'Last;

   subtype Array_Subtype is
     Array_Type (Index_Subtype);

   procedure Sort is
      new Generic_Constrained_Array_Sort
       (Index_Type   => Index_Subtype,
        Element_Type => Element_Type,
        Array_Type   => Array_Subtype,
        "<"          => "<");

begin
   Sort (Container);
end Ada.Containers.Generic_Array_Sort;
