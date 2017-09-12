------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            S Y S T E M . I O                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

package body System.IO is

   Current_Out : File_Type := Stdout;
   pragma Atomic (Current_Out);
   --  Current output file (modified by Set_Output)

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Spacing : Positive := 1) is
   begin
      for J in 1 .. Spacing loop
         Put (ASCII.LF);
      end loop;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (X : Integer) is
      procedure Put_Int (X : Integer);
      pragma Import (C, Put_Int, "put_int");

      procedure Put_Int_Err (X : Integer);
      pragma Import (C, Put_Int_Err, "put_int_stderr");

   begin
      case Current_Out is
         when Stdout => Put_Int (X);
         when Stderr => Put_Int_Err (X);
      end case;
   end Put;

   procedure Put (C : Character) is
      procedure Put_Char (C : Character);
      pragma Import (C, Put_Char, "put_char");

      procedure Put_Char_Stderr (C : Character);
      pragma Import (C, Put_Char_Stderr, "put_char_stderr");

   begin
      case Current_Out is
         when Stdout => Put_Char (C);
         when Stderr => Put_Char_Stderr (C);
      end case;
   end Put;

   procedure Put (S : String) is
   begin
      for J in S'Range loop
         Put (S (J));
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      Put (S);
      New_Line;
   end Put_Line;

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

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (File : File_Type) is
   begin
      Current_Out := File;
   end Set_Output;

end System.IO;
