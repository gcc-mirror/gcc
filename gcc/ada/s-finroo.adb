------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                S Y S T E M . F I N A L I Z A T I O N _ R O O T           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1992-2003 Free Software Foundation, Inc.         --
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

package body System.Finalization_Root is

   --  It should not be possible to call any of these subprograms

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Root_Controlled) is
   begin
      raise Program_Error;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Root_Controlled) is
   begin
      raise Program_Error;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Root_Controlled) is
   begin
      raise Program_Error;
   end Initialize;

   ----------
   -- Read --
   ----------

   --  Read and Write must be empty in order to avoid copying the
   --  finalization pointers.

   pragma Warnings (Off);
   --  Suppress warning for out paramater Item which is not assigned
   --  because it is pretty much empty.

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   Item : out Root_Controlled)
   is
   begin
      null;
   end Read;

   -----------
   -- Write --
   -----------

   --  Read and Write must be empty in order to avoid copying the
   --  finalization pointers.

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    Item : in Root_Controlled)
   is
   begin
      null;
   end Write;

end System.Finalization_Root;
