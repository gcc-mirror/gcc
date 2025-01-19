------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G E N _ I L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2020-2025, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package body Gen_IL is

   procedure Put (F : File_Type; S : String);
   --  The output primitive

   -----------
   -- Image --
   -----------

   function Image (X : Root_Int) return String is
      Result : constant String := X'Img;
   begin
      if Result (1) = ' ' then
         return Result (2 .. Result'Last);
      else
         return Result;
      end if;
   end Image;

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
      Cap : Boolean := True;
   begin
      for X of S loop
         declare
            Old : constant Character := X;
         begin
            if Cap then
               X := To_Upper (X);
            else
               X := To_Lower (X);
            end if;

            Cap := not (Is_Letter (Old) or else Is_Digit (Old));
         end;
      end loop;
   end Capitalize;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (S : String) return String is
   begin
      return Result : String (S'Range) := S do
         Capitalize (Result);
      end return;
   end Capitalize;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File (Buffer : in out Sink; Name : String) is
   begin
      Create (Buffer.File, Out_File, Name);
      Buffer.Indent := 0;
      Buffer.New_Line := True;
   end Create_File;

   ---------------------
   -- Increase_Indent --
   ---------------------

   procedure Increase_Indent (Buffer : in out Sink; Amount : Natural) is
   begin
      Buffer.Indent := Buffer.Indent + Amount;
   end Increase_Indent;

   ---------------------
   -- Decrease_Indent --
   ---------------------

   procedure Decrease_Indent (Buffer : in out Sink; Amount : Natural) is
   begin
      Buffer.Indent := Buffer.Indent - Amount;
   end Decrease_Indent;

   ---------
   -- Put --
   ---------

   procedure Put (F : File_Type; S : String) is
   begin
      String'Write (Stream (F), S);
   end Put;

   procedure Put (Buffer : in out Sink; Item : String) is
   begin
      --  If the first character is LF, indent after it only

      if Item (Item'First) = ASCII.LF then
         Put (Buffer.File, LF);
         Buffer.New_Line := True;

         if Item'Length > 1 then
            Put (Buffer, Item (Item'First + 1 .. Item'Last));
         end if;

         return;
      end if;

      --  If this is a new line, indent

      if Buffer.New_Line and then Buffer.Indent > 0 then
         declare
            S : constant String (1 .. Buffer.Indent) := (others => ' ');
         begin
            Put (Buffer.File, S);
         end;
      end if;

      Put (Buffer.File, Item);

      Buffer.New_Line := Item (Item'Last) = ASCII.LF;
   end Put;

end Gen_IL;
