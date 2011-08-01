------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                              G N A T . I O                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2010, AdaCore                     --
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

package body GNAT.IO is

   Current_Out : File_Type := Stdout;
   pragma Atomic (Current_Out);
   --  Current output file (modified by Set_Output)

   ---------
   -- Get --
   ---------

   procedure Get (X : out Integer) is
      function Get_Int return Integer;
      pragma Import (C, Get_Int, "get_int");
   begin
      X := Get_Int;
   end Get;

   procedure Get (C : out Character) is
      function Get_Char return Character;
      pragma Import (C, Get_Char, "get_char");
   begin
      C := Get_Char;
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (Item : out String; Last : out Natural) is
      C : Character;

   begin
      for Nstore in Item'Range loop
         Get (C);

         if C = ASCII.LF then
            Last := Nstore - 1;
            return;

         else
            Item (Nstore) := C;
         end if;
      end loop;

      Last := Item'Last;
   end Get_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (File : File_Type; Spacing : Positive := 1) is
   begin
      for J in 1 .. Spacing loop
         Put (File, ASCII.LF);
      end loop;
   end New_Line;

   procedure New_Line (Spacing : Positive := 1) is
   begin
      New_Line (Current_Out, Spacing);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (X : Integer) is
   begin
      Put (Current_Out, X);
   end Put;

   procedure Put (File : File_Type; X : Integer) is
      procedure Put_Int (X : Integer);
      pragma Import (C, Put_Int, "put_int");

      procedure Put_Int_Stderr (X : Integer);
      pragma Import (C, Put_Int_Stderr, "put_int_stderr");

   begin
      case File is
         when Stdout => Put_Int (X);
         when Stderr => Put_Int_Stderr (X);
      end case;
   end Put;

   procedure Put (C : Character) is
   begin
      Put (Current_Out, C);
   end Put;

   procedure Put (File : File_Type; C : Character) is
      procedure Put_Char (C : Character);
      pragma Import (C, Put_Char, "put_char");

      procedure Put_Char_Stderr (C : Character);
      pragma Import (C, Put_Char_Stderr, "put_char_stderr");

   begin
      case File is
         when Stdout => Put_Char (C);
         when Stderr => Put_Char_Stderr (C);
      end case;
   end Put;

   procedure Put (S : String) is
   begin
      Put (Current_Out, S);
   end Put;

   procedure Put (File : File_Type; S : String) is
   begin
      for J in S'Range loop
         Put (File, S (J));
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      Put_Line (Current_Out, S);
   end Put_Line;

   procedure Put_Line (File : File_Type; S : String) is
   begin
      Put (File, S);
      New_Line (File);
   end Put_Line;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (File : File_Type) is
   begin
      Current_Out := File;
   end Set_Output;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Type is
   begin
      return Stdout;
   end Standard_Output;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Type is
   begin
      return Stderr;
   end Standard_Error;

end GNAT.IO;
