------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Prj.Err;
with Sinput.C;

package body Sinput.P is

   First : Boolean := True;
   --  Flag used when Load_Project_File is called the first time,
   --  to set Main_Source_File.
   --  The flag is reset to False at the first call to Load_Project_File

   -----------------------
   -- Load_Project_File --
   -----------------------

   function Load_Project_File (Path : String) return Source_File_Index is
      X    : Source_File_Index;

   begin
      X := Sinput.C.Load_File (Path);

      if First then
         Main_Source_File := X;
         First := False;
      end if;

      return X;
   end Load_Project_File;

   --------------------------------
   -- Restore_Project_Scan_State --
   --------------------------------

   procedure Restore_Project_Scan_State
     (Saved_State : in Saved_Project_Scan_State)
   is
   begin
      Restore_Scan_State (Saved_State.Scan_State);
      Source              := Saved_State.Source;
      Current_Source_File := Saved_State.Current_Source_File;
   end Restore_Project_Scan_State;

   -----------------------------
   -- Save_Project_Scan_State --
   -----------------------------

   procedure Save_Project_Scan_State
     (Saved_State : out Saved_Project_Scan_State)
   is
   begin
      Save_Scan_State (Saved_State.Scan_State);
      Saved_State.Source              := Source;
      Saved_State.Current_Source_File := Current_Source_File;
   end Save_Project_Scan_State;

   ----------------------------
   -- Source_File_Is_Subunit --
   ----------------------------

   function Source_File_Is_Subunit (X : Source_File_Index) return Boolean is
   begin
      Prj.Err.Scanner.Initialize_Scanner (No_Unit, X);

      --  We scan past junk to the first interesting compilation unit
      --  token, to see if it is SEPARATE. We ignore WITH keywords during
      --  this and also PRIVATE. The reason for ignoring PRIVATE is that
      --  it handles some error situations, and also it is possible that
      --  a PRIVATE WITH feature might be approved some time in the future.

      while Token = Tok_With
        or else Token = Tok_Private
        or else (Token not in Token_Class_Cunit and then Token /= Tok_EOF)
      loop
         Prj.Err.Scanner.Scan;
      end loop;

      return Token = Tok_Separate;
   end Source_File_Is_Subunit;

end Sinput.P;
