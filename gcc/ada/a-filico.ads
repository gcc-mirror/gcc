------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     A D A . F I N A L I Z A T I O N . L I S T _ C O N T R O L L E R      --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                            $Revision: 1.1.16.1 $
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

with System.Finalization_Root;
package Ada.Finalization.List_Controller is
pragma Elaborate_Body (List_Controller);

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

   --  Management of a bidirectional linked heterogenous list of
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
