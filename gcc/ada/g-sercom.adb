------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2007-2008, AdaCore                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Default version of this package

with Ada.Streams; use Ada.Streams;

package body GNAT.Serial_Communications is

   pragma Warnings (Off);
   --  Kill warnings on unreferenced formals

   type Port_Data is new Integer;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Unimplemented;
   pragma No_Return (Unimplemented);
   --  This procedure raises a Program_Error with an appropriate message
   --  indicating that an unimplemented feature has been used.

   ----------
   -- Name --
   ----------

   function Name (Number : Positive) return Port_Name is
   begin
      Unimplemented;
      return "";
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (Port : out Serial_Port;
      Name : Port_Name) is
   begin
      Unimplemented;
   end Open;

   ---------
   -- Set --
   ---------

   procedure Set
     (Port      : Serial_Port;
      Rate      : Data_Rate        := B9600;
      Bits      : Data_Bits        := CS8;
      Stop_Bits : Stop_Bits_Number := One;
      Parity    : Parity_Check     := None;
      Block     : Boolean          := True;
      Timeout   : Duration         := 10.0) is
   begin
      Unimplemented;
   end Set;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Port   : in out Serial_Port;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Unimplemented;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Port   : in out Serial_Port;
      Buffer : Stream_Element_Array) is
   begin
      Unimplemented;
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (Port : in out Serial_Port) is
   begin
      Unimplemented;
   end Close;

   -------------------
   -- Unimplemented; --
   -------------------

   procedure Unimplemented is
   begin
      raise Program_Error with "Serial_Communications not implemented";
   end Unimplemented;

end GNAT.Serial_Communications;
