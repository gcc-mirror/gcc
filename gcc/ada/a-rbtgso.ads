------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S .      --
--               G E N E R I C _ S E T _ O P E R A T I O N S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  Tree_Type is used to implement ordered containers. This package declares
--  set-based tree operations.

with Ada.Containers.Red_Black_Trees.Generic_Operations;

generic
   with package Tree_Operations is new Generic_Operations (<>);

   use Tree_Operations.Tree_Types;

   with procedure Insert_With_Hint
     (Dst_Tree : in out Tree_Type;
      Dst_Hint : Node_Access;
      Src_Node : Node_Access;
      Dst_Node : out Node_Access);

   with function Copy_Tree (Source_Root : Node_Access)
       return Node_Access;

   with procedure Delete_Tree (X : in out Node_Access);

   with function Is_Less (Left, Right : Node_Access) return Boolean;

   with procedure Free (X : in out Node_Access);

package Ada.Containers.Red_Black_Trees.Generic_Set_Operations is
   pragma Pure;

   procedure Union (Target : in out Tree_Type; Source : Tree_Type);

   function Union (Left, Right : Tree_Type) return Tree_Type;

   procedure Intersection (Target : in out Tree_Type; Source : Tree_Type);

   function Intersection (Left, Right : Tree_Type) return Tree_Type;

   procedure Difference (Target : in out Tree_Type; Source : Tree_Type);

   function Difference (Left, Right : Tree_Type) return Tree_Type;

   procedure Symmetric_Difference
     (Target : in out Tree_Type;
      Source : Tree_Type);

   function Symmetric_Difference (Left, Right : Tree_Type) return Tree_Type;

   function Is_Subset (Subset : Tree_Type; Of_Set : Tree_Type) return Boolean;

   function Overlap (Left, Right : Tree_Type) return Boolean;

end Ada.Containers.Red_Black_Trees.Generic_Set_Operations;
