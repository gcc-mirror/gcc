------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2019, Free Software Foundation, Inc.         --
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

with Ada.Containers.Helpers;

package Ada.Containers.Red_Black_Trees is
   pragma Pure;

   type Color_Type is (Red, Black);

   generic
      type Node_Type (<>) is limited private;
      type Node_Access is access Node_Type;
   package Generic_Tree_Types is

      type Tree_Type is tagged record
         First  : Node_Access := null;
         Last   : Node_Access := null;
         Root   : Node_Access := null;
         Length : Count_Type := 0;
         TC     : aliased Helpers.Tamper_Counts;
      end record;

      package Implementation is new Helpers.Generic_Implementation;
   end Generic_Tree_Types;

   generic
      type Node_Type is private;
   package Generic_Bounded_Tree_Types is
      type Nodes_Type is array (Count_Type range <>) of Node_Type;

      --  Note that objects of type Tree_Type are logically initialized (in the
      --  sense that representation invariants of type are satisfied by dint of
      --  default initialization), even without the Nodes component also having
      --  its own initialization expression. We only initializae the Nodes
      --  component here in order to prevent spurious compiler warnings about
      --  the container object not being fully initialized.

      type Tree_Type (Capacity : Count_Type) is tagged record
         First  : Count_Type := 0;
         Last   : Count_Type := 0;
         Root   : Count_Type := 0;
         Length : Count_Type := 0;
         TC     : aliased Helpers.Tamper_Counts;
         Free   : Count_Type'Base := -1;
         Nodes  : Nodes_Type (1 .. Capacity) := (others => <>);
      end record;

      package Implementation is new Helpers.Generic_Implementation;
   end Generic_Bounded_Tree_Types;

end Ada.Containers.Red_Black_Trees;
