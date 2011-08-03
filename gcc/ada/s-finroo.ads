------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                S Y S T E M . F I N A L I Z A T I O N _ R O O T           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

--  This unit provides the basic support for controlled (finalizable) types

with Ada.Streams;

package System.Finalization_Root is
   pragma Preelaborate;

   --  The base for types Controlled and Limited_Controlled declared in Ada.
   --  Finalization.

   type Root_Controlled is tagged null record;

   procedure Adjust     (Object : in out Root_Controlled);
   procedure Finalize   (Object : in out Root_Controlled);
   procedure Initialize (Object : in out Root_Controlled);

   package Stream_Attributes is
      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Root_Controlled) is null;

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Root_Controlled) is null;
   end Stream_Attributes;

   for Root_Controlled'Read  use Stream_Attributes.Read;
   for Root_Controlled'Write use Stream_Attributes.Write;
end System.Finalization_Root;
