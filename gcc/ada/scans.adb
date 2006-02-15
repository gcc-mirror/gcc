------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S C A N S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

with Namet;  use Namet;
with Snames; use Snames;

package body Scans is

   -----------------------------
   -- Initialize_Ada_Keywords --
   -----------------------------

   procedure Initialize_Ada_Keywords is
      procedure Set_Reserved (N : Name_Id; T : Token_Type);
      pragma Inline (Set_Reserved);
      --  Set given name as a reserved word (T is the corresponding token)

      ------------------
      -- Set_Reserved --
      ------------------

      procedure Set_Reserved (N : Name_Id; T : Token_Type) is
      begin
         --  Set up Token_Type values in Names table entries for reserved
         --  words. We use the Pos value of the Token_Type value. Note that
         --  Is_Keyword_Name relies on the fact that Token_Type'Val (0) is not
         --  a reserved word!

         Set_Name_Table_Byte (N, Token_Type'Pos (T));
      end Set_Reserved;

   --  Start of processing for Initialize_Ada_Keywords

   begin
      --  Establish reserved words

      Set_Reserved (Name_Abort,     Tok_Abort);
      Set_Reserved (Name_Abs,       Tok_Abs);
      Set_Reserved (Name_Abstract,  Tok_Abstract);
      Set_Reserved (Name_Accept,    Tok_Accept);
      Set_Reserved (Name_Access,    Tok_Access);
      Set_Reserved (Name_And,       Tok_And);
      Set_Reserved (Name_Aliased,   Tok_Aliased);
      Set_Reserved (Name_All,       Tok_All);
      Set_Reserved (Name_Array,     Tok_Array);
      Set_Reserved (Name_At,        Tok_At);
      Set_Reserved (Name_Begin,     Tok_Begin);
      Set_Reserved (Name_Body,      Tok_Body);
      Set_Reserved (Name_Case,      Tok_Case);
      Set_Reserved (Name_Constant,  Tok_Constant);
      Set_Reserved (Name_Declare,   Tok_Declare);
      Set_Reserved (Name_Delay,     Tok_Delay);
      Set_Reserved (Name_Delta,     Tok_Delta);
      Set_Reserved (Name_Digits,    Tok_Digits);
      Set_Reserved (Name_Do,        Tok_Do);
      Set_Reserved (Name_Else,      Tok_Else);
      Set_Reserved (Name_Elsif,     Tok_Elsif);
      Set_Reserved (Name_End,       Tok_End);
      Set_Reserved (Name_Entry,     Tok_Entry);
      Set_Reserved (Name_Exception, Tok_Exception);
      Set_Reserved (Name_Exit,      Tok_Exit);
      Set_Reserved (Name_For,       Tok_For);
      Set_Reserved (Name_Function,  Tok_Function);
      Set_Reserved (Name_Generic,   Tok_Generic);
      Set_Reserved (Name_Goto,      Tok_Goto);
      Set_Reserved (Name_If,        Tok_If);
      Set_Reserved (Name_In,        Tok_In);
      Set_Reserved (Name_Is,        Tok_Is);
      Set_Reserved (Name_Limited,   Tok_Limited);
      Set_Reserved (Name_Loop,      Tok_Loop);
      Set_Reserved (Name_Mod,       Tok_Mod);
      Set_Reserved (Name_New,       Tok_New);
      Set_Reserved (Name_Not,       Tok_Not);
      Set_Reserved (Name_Null,      Tok_Null);
      Set_Reserved (Name_Of,        Tok_Of);
      Set_Reserved (Name_Or,        Tok_Or);
      Set_Reserved (Name_Others,    Tok_Others);
      Set_Reserved (Name_Out,       Tok_Out);
      Set_Reserved (Name_Package,   Tok_Package);
      Set_Reserved (Name_Pragma,    Tok_Pragma);
      Set_Reserved (Name_Private,   Tok_Private);
      Set_Reserved (Name_Procedure, Tok_Procedure);
      Set_Reserved (Name_Protected, Tok_Protected);
      Set_Reserved (Name_Raise,     Tok_Raise);
      Set_Reserved (Name_Range,     Tok_Range);
      Set_Reserved (Name_Record,    Tok_Record);
      Set_Reserved (Name_Rem,       Tok_Rem);
      Set_Reserved (Name_Renames,   Tok_Renames);
      Set_Reserved (Name_Requeue,   Tok_Requeue);
      Set_Reserved (Name_Return,    Tok_Return);
      Set_Reserved (Name_Reverse,   Tok_Reverse);
      Set_Reserved (Name_Select,    Tok_Select);
      Set_Reserved (Name_Separate,  Tok_Separate);
      Set_Reserved (Name_Subtype,   Tok_Subtype);
      Set_Reserved (Name_Tagged,    Tok_Tagged);
      Set_Reserved (Name_Task,      Tok_Task);
      Set_Reserved (Name_Terminate, Tok_Terminate);
      Set_Reserved (Name_Then,      Tok_Then);
      Set_Reserved (Name_Type,      Tok_Type);
      Set_Reserved (Name_Until,     Tok_Until);
      Set_Reserved (Name_Use,       Tok_Use);
      Set_Reserved (Name_When,      Tok_When);
      Set_Reserved (Name_While,     Tok_While);
      Set_Reserved (Name_With,      Tok_With);
      Set_Reserved (Name_Xor,       Tok_Xor);

      --  Ada 2005 reserved words

      Set_Reserved (Name_Interface,     Tok_Interface);
      Set_Reserved (Name_Overriding,    Tok_Overriding);
      Set_Reserved (Name_Synchronized,  Tok_Synchronized);

   end Initialize_Ada_Keywords;

   ------------------------
   -- Restore_Scan_State --
   ------------------------

   procedure Restore_Scan_State (Saved_State : Saved_Scan_State) is
   begin
      Scan_Ptr                 := Saved_State.Save_Scan_Ptr;
      Token                    := Saved_State.Save_Token;
      Token_Ptr                := Saved_State.Save_Token_Ptr;
      Current_Line_Start       := Saved_State.Save_Current_Line_Start;
      Start_Column             := Saved_State.Save_Start_Column;
      Checksum                 := Saved_State.Save_Checksum;
      First_Non_Blank_Location := Saved_State.Save_First_Non_Blank_Location;
      Token_Node               := Saved_State.Save_Token_Node;
      Token_Name               := Saved_State.Save_Token_Name;
      Prev_Token               := Saved_State.Save_Prev_Token;
      Prev_Token_Ptr           := Saved_State.Save_Prev_Token_Ptr;
   end Restore_Scan_State;

   ---------------------
   -- Save_Scan_State --
   ---------------------

   procedure Save_Scan_State (Saved_State : out Saved_Scan_State) is
   begin
      Saved_State.Save_Scan_Ptr                 := Scan_Ptr;
      Saved_State.Save_Token                    := Token;
      Saved_State.Save_Token_Ptr                := Token_Ptr;
      Saved_State.Save_Current_Line_Start       := Current_Line_Start;
      Saved_State.Save_Start_Column             := Start_Column;
      Saved_State.Save_Checksum                 := Checksum;
      Saved_State.Save_First_Non_Blank_Location := First_Non_Blank_Location;
      Saved_State.Save_Token_Node               := Token_Node;
      Saved_State.Save_Token_Name               := Token_Name;
      Saved_State.Save_Prev_Token               := Prev_Token;
      Saved_State.Save_Prev_Token_Ptr           := Prev_Token_Ptr;
   end Save_Scan_State;

end Scans;
