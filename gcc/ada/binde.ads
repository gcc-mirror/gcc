------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines to determine elaboration order

with ALI;   use ALI;
with Table;
with Types; use Types;

package Binde is

   --  The following table records the chosen elaboration order. It is used
   --  by Gen_Elab_Call to generate the sequence of elaboration calls. Note
   --  that units are included in this table even if they have no elaboration
   --  routine, since the table is also used to drive the generation of object
   --  files in the binder output. Gen_Elab_Call skips any units that have no
   --  elaboration routine.

   package Elab_Order is new Table.Table (
     Table_Component_Type => Unit_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 500,
     Table_Increment      => 200,
     Table_Name           => "Elab_Order");

   procedure Find_Elab_Order;
   --  Determine elaboration order

end Binde;
