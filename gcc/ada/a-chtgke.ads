------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      A D A . C O N T A I N E R S .                       --
--             H A S H _ T A B L E S . G E N E R I C _ K E Y S              --
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

generic
   with package HT_Types is
     new Generic_Hash_Table_Types (<>);

   use HT_Types;

   with function Next (Node : Node_Access) return Node_Access;

   with procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access);

   type Key_Type (<>) is limited private;

   with function Hash (Key : Key_Type) return Hash_Type;

   with function Equivalent_Keys
     (Key  : Key_Type;
      Node : Node_Access) return Boolean;

package Ada.Containers.Hash_Tables.Generic_Keys is
   pragma Preelaborate;

   function Index
     (HT  : Hash_Table_Type;
      Key : Key_Type) return Hash_Type;
   pragma Inline (Index);

   procedure Delete_Key_Sans_Free
     (HT   : in out Hash_Table_Type;
      Key  : Key_Type;
      X    : out Node_Access);

   function Find (HT  : Hash_Table_Type; Key : Key_Type) return Node_Access;

   generic
      with function New_Node
        (Next : Node_Access) return Node_Access;
   procedure Generic_Conditional_Insert
     (HT       : in out Hash_Table_Type;
      Key      : Key_Type;
      Node     : out Node_Access;
      Inserted : out Boolean);

end Ada.Containers.Hash_Tables.Generic_Keys;
