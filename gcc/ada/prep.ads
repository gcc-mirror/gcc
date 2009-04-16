------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 P R E P                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2008, Free Software Foundation, Inc.         --
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

with GNAT.Dynamic_Tables;

with Namet; use Namet;
with Types; use Types;

package Prep is

   -----------------
   -- Symbol Data --
   -----------------

   type Symbol_Data is record
      Symbol : Name_Id := No_Name;
      --  The symbol in lower case

      Original : Name_Id := No_Name;
      --  The symbol as originally given in the definition file or on
      --  the command line.

      On_The_Command_Line : Boolean := False;
      --  Set to True if symbol is defined on the command line.
      --  Used to prevent replacement of command line symbols by definition
      --  file symbols.

      Is_A_String : Boolean := False;
      --  Indicate if the value of the symbol has been specified as a string
      --  or simply as a sequence of characters.

      Value : String_Id := No_String;
      --  The value of the symbol (string or sequence of characters)

   end record;

   True_Value : Symbol_Data :=
     (Symbol              => No_Name,
      Original            => No_Name,
      On_The_Command_Line => False,
      Is_A_String         => False,
      Value               => No_String);

   type Symbol_Id is new Nat;
   No_Symbol : constant Symbol_Id := 0;

   package Symbol_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Symbol_Data,
      Table_Index_Type     => Symbol_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  The table of all symbols

   Mapping : Symbol_Table.Instance;
   --  The mapping table of symbols to values used by procedure Parse_Def_File
   --  and Preprocess.

   function Index_Of (Symbol : Name_Id) return Symbol_Id;
   --  Return the index in the Mapping table of Symbol.
   --  Return No_Symbol if Symbol in not in the Mapping table.

   --  Access to procedure types used by procedure Initialize below:

   type Error_Msg_Proc is access procedure
     (Msg : String; Flag_Location : Source_Ptr);

   type Scan_Proc is access procedure;

   type Set_Ignore_Errors_Proc is access procedure (To : Boolean);

   type Put_Char_Proc is access procedure (C : Character);

   type New_EOL_Proc is access procedure;

   procedure Initialize;
   --  Initialize the preprocessor's global structures

   procedure Setup_Hooks
     (Error_Msg         : Error_Msg_Proc;
      Scan              : Scan_Proc;
      Set_Ignore_Errors : Set_Ignore_Errors_Proc;
      Put_Char          : Put_Char_Proc;
      New_EOL           : New_EOL_Proc);
   --  Set the i/o hooks used by the preprocessor

   procedure Parse_Def_File;
   --  Parse the definition file. The definition file must have already been
   --  loaded and the scanner initialized.

   procedure Preprocess (Source_Modified : out Boolean);
   --  Preprocess the input file. The input file must have already been loaded
   --  and the scanner initialized. Source_Modified is set to True iff the
   --  preprocessor modified the source text.

   procedure Check_Command_Line_Symbol_Definition
     (Definition  : String;
      Data        : out Symbol_Data);
   --  Check the validity of a command line definition <symbol>=<value>.
   --  Return the symbol and its value in Data if the definition is valid,
   --  fail if it is not valid.

   procedure Change_Reserved_Keyword_To_Symbol
     (All_Keywords : Boolean := False);
   --  If Token is an Ada reserved word (other than IF, ELSIF, ELSE,
   --  END, AND, OR, THEN when All_Keywords is False), change it to
   --  Tok_Identifier with the corresponding Token_Name.

   procedure List_Symbols (Foreword : String);
   --  List the symbols used for preprocessing a file, with their values.
   --  If Foreword is not empty, Output Foreword before the list.

end Prep;
