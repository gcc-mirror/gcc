------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     A D A . F I N A L I Z A T I O N . L I S T _ C O N T R O L L E R      --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

with System.Finalization_Root;

package Ada.Finalization.List_Controller is
   pragma Elaborate_Body;

   package SFR renames System.Finalization_Root;

   ----------------------------
   -- Simple_List_Controller --
   ----------------------------

   type Simple_List_Controller is new Ada.Finalization.Limited_Controlled
     with record
        F : SFR.Finalizable_Ptr;
     end record;
      --  Used by the compiler to carry a list of temporary objects that
      --  needs to be finalized after having being used. This list is
      --  embedded in a controlled type so that if an exception is raised
      --  while those temporaries are still in use, they will be reclaimed
      --  by the normal finalization mechanism.

   procedure Finalize (Object : in out Simple_List_Controller);

   ---------------------
   -- List_Controller --
   ---------------------

   --  Management of a bidirectional linked heterogeneous list of
   --  dynamically Allocated objects. To simplify the management of the
   --  linked list, the First and Last elements are statically part of the
   --  original List controller:
   --
   --        +------------+
   --        |          --|-->--
   --        +------------+
   --        |--<--       |                      record with ctrl components
   --        |------------|                         +----------+
   --     +--|--   L      |                         |          |
   --     |  |------------|                         |          |
   --     |  |+--------+  |       +--------+        |+--------+|
   --     +->||  prev  | F|---<---|--      |----<---||--      ||--<--+
   --        ||--------| i|       |--------|        ||--------||     |
   --        || next   | r|--->---|      --|---->---||      --||--------+
   --        |+--------+ s|       |--------|        ||--------||     |  |
   --        |           t|       | ctrl   |        ||        ||     |  |
   --        |            |       :        :        |+--------+|     |  |
   --        |            |       : object :        |rec       |     |  |
   --        |            |       :        :        |controller|     |  |
   --        |            |       |        |        |          |     |  v
   --        |+--------+  |       +--------+        +----------+     |  |
   --        ||  prev -|-L|--------------------->--------------------+  |
   --        ||--------| a|                                             |
   --        || next   | s|-------------------<-------------------------+
   --        |+--------+ t|
   --        |            |
   --        +------------+

   type List_Controller is new Ada.Finalization.Limited_Controlled
     with record
        F    :  SFR.Finalizable_Ptr;
        First,
        Last : aliased SFR.Root_Controlled;
     end record;
   --  Controls the chains of dynamically allocated controlled
   --  objects makes sure that they get finalized upon exit from
   --  the access type that defined them

   procedure Initialize (Object : in out List_Controller);
   procedure Finalize   (Object : in out List_Controller);

end Ada.Finalization.List_Controller;
