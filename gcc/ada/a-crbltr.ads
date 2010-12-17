------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2010, Free Software Foundation, Inc.         --
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

--  This package declares the tree type used to implement ordered containers

package Ada.Containers.Red_Black_Trees is
   pragma Pure;

   type Color_Type is (Red, Black);

   generic
      type Node_Type (<>) is limited private;
      type Node_Access is access Node_Type;
   package Generic_Tree_Types is
      type Tree_Type is tagged record
         First  : Node_Access;
         Last   : Node_Access;
         Root   : Node_Access;
         Length : Count_Type := 0;
         Busy   : Natural := 0;
         Lock   : Natural := 0;
      end record;
   end Generic_Tree_Types;

   generic
      type Node_Type is private;
   package Generic_Bounded_Tree_Types is
      type Nodes_Type is array (Count_Type range <>) of Node_Type;

      type Tree_Type (Capacity : Count_Type) is tagged record
         First  : Count_Type := 0;
         Last   : Count_Type := 0;
         Root   : Count_Type := 0;
         Length : Count_Type := 0;
         Busy   : Natural := 0;
         Lock   : Natural := 0;
         Free   : Count_Type'Base := -1;
         Nodes  : Nodes_Type (1 .. Capacity);
      end record;
   end Generic_Bounded_Tree_Types;

end Ada.Containers.Red_Black_Trees;
