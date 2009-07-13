------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E R R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2009, Free Software Foundation, Inc.         --
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

with Output;  use Output;
with Stringt; use Stringt;

package body Prj.Err is

   -----------------------
   -- Obsolescent_Check --
   -----------------------

   procedure Obsolescent_Check (S : Source_Ptr) is
      pragma Warnings (Off, S);
   begin
      null;
   end Obsolescent_Check;

   ---------------
   -- Post_Scan --
   ---------------

   procedure Post_Scan is
      Debug_Tokens : constant Boolean := False;

   begin
      --  Change operator symbol to literal strings, since that's the way
      --  we treat all strings in a project file.

      if Token = Tok_Operator_Symbol
        or else Token = Tok_String_Literal
      then
         Token := Tok_String_Literal;
         String_To_Name_Buffer (String_Literal_Id);
         Token_Name := Name_Find;
      end if;

      if Debug_Tokens then
         Write_Line (Token_Type'Image (Token));

         if Token = Tok_Identifier
           or else Token = Tok_String_Literal
         then
            Write_Line ("  " & Get_Name_String (Token_Name));
         end if;
      end if;
   end Post_Scan;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg
     (Flags    : Processing_Flags;
      Msg      : String;
      Location : Source_Ptr := No_Location;
      Project  : Project_Id := null)
   is
      Real_Location : Source_Ptr := Location;

   begin
      --  Display the error message in the traces so that it appears in the
      --  correct location in the traces (otherwise error messages are only
      --  displayed at the end and it is difficult to see when they were
      --  triggered)

      if Current_Verbosity = High then
         Write_Line ("ERROR: " & Msg);
      end if;

      --  If location of error is unknown, use the location of the project

      if Real_Location = No_Location
        and then Project /= null
      then
         Real_Location := Project.Location;
      end if;

      if Real_Location = No_Location then

         --  If still null, we are parsing a project that was created in-memory
         --  so we shouldn't report errors for projects that the user has no
         --  access to in any case.

         return;
      end if;

      --  Report the error through Errutil, so that duplicate errors are
      --  properly removed, messages are sorted, and correctly interpreted,...

      Errutil.Error_Msg (Msg, Real_Location);

      --  Let the application know there was an error

      if Flags.Report_Error /= null then
         Flags.Report_Error
           (Project,
            Is_Warning => Msg (Msg'First) = '?' or else Msg (Msg'First) = '<');
      end if;
   end Error_Msg;

end Prj.Err;
