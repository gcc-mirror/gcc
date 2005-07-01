------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--        A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S .     --
--                    G E N E R I C _ O P E R A T I O N S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Ada.Streams; use Ada.Streams;

generic
   with package Tree_Types is new Generic_Tree_Types (<>);
   use Tree_Types;

   with function  Parent (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Parent (Node : Node_Access; Parent : Node_Access) is <>;
   with function  Left (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Left (Node : Node_Access; Left : Node_Access) is <>;
   with function  Right (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Right (Node : Node_Access; Right : Node_Access) is <>;
   with function  Color (Node : Node_Access) return Color_Type is <>;
   with procedure Set_Color (Node : Node_Access; Color : Color_Type) is <>;

package Ada.Containers.Red_Black_Trees.Generic_Operations is
pragma Pure;

   function Min (Node : Node_Access) return Node_Access;

   function Max (Node : Node_Access) return Node_Access;

   procedure Check_Invariant (Tree : Tree_Type);

   function Next (Node : Node_Access) return Node_Access;

   function Previous (Node : Node_Access) return Node_Access;

   generic
      with function Is_Equal (L, R : Node_Access) return Boolean;
   function Generic_Equal (Left, Right : Tree_Type) return Boolean;

   procedure Delete_Node_Sans_Free
     (Tree : in out Tree_Type;
      Node : Node_Access);

   generic
      with procedure Free (X : in out Node_Access);
   procedure Generic_Delete_Tree (X : in out Node_Access);

   generic
      with function Copy_Node (Source : Node_Access) return Node_Access;
      with procedure Delete_Tree (X : in out Node_Access);
   function Generic_Copy_Tree (Source_Root : Node_Access) return Node_Access;

   generic
      with function Copy_Tree (Root : Node_Access) return Node_Access;
   procedure Generic_Adjust (Tree : in out Tree_Type);

   generic
      with procedure Delete_Tree (X : in out Node_Access);
   procedure Generic_Clear (Tree : in out Tree_Type);

   generic
      with procedure Clear (Tree : in out Tree_Type);
   procedure Generic_Move (Target, Source : in out Tree_Type);

   generic
      with procedure Process (Node : Node_Access) is <>;
   procedure Generic_Iteration (Tree : Tree_Type);

   generic
      with procedure Process (Node : Node_Access) is <>;
   procedure Generic_Reverse_Iteration (Tree : Tree_Type);

   generic
      with procedure Write_Node
        (Stream : access Root_Stream_Type'Class;
         Node   : Node_Access);
   procedure Generic_Write
     (Stream : access Root_Stream_Type'Class;
      Tree   : Tree_Type);

   generic
      with procedure Clear (Tree : in out Tree_Type);
      with function Read_Node
        (Stream : access Root_Stream_Type'Class) return Node_Access;
   procedure Generic_Read
     (Stream : access Root_Stream_Type'Class;
      Tree   : in out Tree_Type);

   procedure Rebalance_For_Insert
     (Tree : in out Tree_Type;
      Node : Node_Access);

end Ada.Containers.Red_Black_Trees.Generic_Operations;
