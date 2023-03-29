------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T Y L E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  This package collects all the routines used for style checking in the
--  compiler, as activated by the relevant command line option. These are
--  gathered in a separate package so that they can more easily be customized.
--  Calls to these subprograms are only made if Opt.Style_Check is set True.

with Errout;
with Styleg;
with Types;    use Types;

package Style is

   procedure Body_With_No_Spec (N : Node_Id);
   --  Called where N is a subprogram body node for a subprogram body
   --  for which no spec was given, i.e. a body acting as its own spec.

   procedure Check_Array_Attribute_Index
     (N  : Node_Id;
      E1 : Node_Id;
      D  : Int);
   --  Called for an array attribute specifying an index number. N is the
   --  node for the attribute, and E1 is the index expression (Empty if none
   --  present). If E1 is present, it is known to be a static integer. D is
   --  the number of dimensions of the array.

   procedure Check_Identifier
     (Ref : Node_Or_Entity_Id;
      Def : Node_Or_Entity_Id);
   --  Check style of identifier occurrence. Ref is an N_Identifier node whose
   --  spelling is to be checked against the Chars spelling in identifier node
   --  Def (which may be either an N_Identifier, or N_Defining_Identifier node)

   procedure Missing_Overriding (N : Node_Id; E : Entity_Id);
   --  Called where N is the declaration or body of an overriding operation,
   --  and the node does not have an overriding_indicator.

   procedure Subprogram_Not_In_Alpha_Order (Name : Node_Id);
   --  Called if Name is the name of a subprogram body in a package body
   --  that is not in alphabetical order.

   --  Remaining style routines come from instantiation of Styleg

   package Style_Inst is new Styleg
     (Errout.Error_Msg,
      Errout.Error_Msg_S,
      Errout.Error_Msg_SC,
      Errout.Error_Msg_SP);
   --  Instantiation of Styleg for compiler use

   procedure Check_Abs_Not
     renames Style_Inst.Check_Abs_Not;
   --  Called after scanning an ABS or NOT operator to check spacing

   procedure Check_Apostrophe
     renames Style_Inst.Check_Apostrophe;
   --  Called after scanning an apostrophe to check spacing

   procedure Check_Arrow (Inside_Depends : Boolean := False)
     renames Style_Inst.Check_Arrow;
   --  Called after scanning out an arrow to check spacing

   procedure Check_Attribute_Name (Reserved : Boolean)
     renames Style_Inst.Check_Attribute_Name;
   --  The current token is an attribute designator. Check that it is
   --  capitalized in an appropriate manner. Reserved is set if the attribute
   --  designator is a reserved word (access, digits, delta or range) to allow
   --  differing rules for the two cases.

   procedure Check_Boolean_Operator (Node : Node_Id)
     renames Style_Inst.Check_Boolean_Operator;
   --  Called after resolving AND or OR node to check short circuit rules

   procedure Check_Box
     renames Style_Inst.Check_Box;
   --  Called after scanning out a box to check spacing

   procedure Check_Binary_Operator
     renames Style_Inst.Check_Binary_Operator;
   --  Called after scanning out a binary operator other than a plus, minus
   --  or exponentiation operator. Intended for checking spacing rules.

   procedure Check_Exponentiation_Operator
     renames Style_Inst.Check_Exponentiation_Operator;
   --  Called after scanning out an exponentiation operator. Intended for
   --  checking spacing rules.

   procedure Check_Colon
     renames Style_Inst.Check_Colon;
   --  Called after scanning out colon to check spacing

   procedure Check_Colon_Equal
     renames Style_Inst.Check_Colon_Equal;
   --  Called after scanning out colon equal to check spacing

   procedure Check_Comma
     renames Style_Inst.Check_Comma;
   --  Called after scanning out comma to check spacing

   procedure Check_Comment
     renames Style_Inst.Check_Comment;
   --  Called with Scan_Ptr pointing to the first minus sign of a comment.
   --  Intended for checking any specific rules for comment placement/format.

   procedure Check_Defining_Identifier_Casing
     renames Style_Inst.Check_Defining_Identifier_Casing;

   procedure Check_Dot_Dot
     renames Style_Inst.Check_Dot_Dot;
   --  Called after scanning out dot dot to check spacing

   procedure Check_EOF
     renames Style_Inst.Check_EOF;
   --  Called after scanning out end of file mark

   procedure Check_HT
     renames Style_Inst.Check_HT;
   --  Called with Scan_Ptr pointing to a horizontal tab character

   procedure Check_Indentation
     renames Style_Inst.Check_Indentation;
   --  Called at the start of a new statement or declaration, with Token_Ptr
   --  pointing to the first token of the statement or declaration. The check
   --  is that the starting column is appropriate to the indentation rules if
   --  Token_Ptr is the first token on the line.

   procedure Check_Left_Paren_Square_Bracket
     renames Style_Inst.Check_Left_Paren_Square_Bracket;
   --  Called after scanning out a left parenthesis to check spacing. If
   --  Ada_Version >= Ada_2022 then called similarly for a left square bracket.

   procedure Check_Line_Terminator (Len : Int)
     renames Style_Inst.Check_Line_Terminator;
   --  Called with Scan_Ptr pointing to the first line terminator terminating
   --  the current line, used to check for appropriate line terminator and to
   --  check the line length (Len is the length of the current line). Note that
   --  the terminator may be the EOF character.

   procedure Check_Not_In
     renames Style_Inst.Check_Not_In;
   --  Called with Scan_Ptr pointing to an IN token, and Prev_Token_Ptr
   --  pointing to a NOT token. Used to check proper layout of NOT IN.

   procedure Check_Pragma_Name
     renames Style_Inst.Check_Pragma_Name;
   --  The current token is a pragma identifier. Check that it is spelled
   --  properly (i.e. with an appropriate casing convention).

   procedure Check_Right_Paren
     renames Style_Inst.Check_Right_Paren;
   --  Called after scanning out a right parenthesis to check spacing

   procedure Check_Semicolon
     renames Style_Inst.Check_Semicolon;
   --  Called after scanning out a semicolon to check spacing

   procedure Check_Then (If_Loc : Source_Ptr)
     renames Style_Inst.Check_Then;
   --  Called to check that THEN and IF keywords are appropriately positioned.
   --  The parameters show the first characters of the two keywords. This
   --  procedure is called only if THEN appears at the start of a line with
   --  Token_Ptr pointing to the THEN keyword.

   procedure Check_Unary_Plus_Or_Minus (Inside_Depends : Boolean := False)
     renames Style_Inst.Check_Unary_Plus_Or_Minus;
   --  Called after scanning a unary plus or minus to check spacing

   procedure Check_Vertical_Bar
     renames Style_Inst.Check_Vertical_Bar;
   --  Called after scanning a vertical bar to check spacing

   procedure Check_Xtra_Parens (Loc : Source_Ptr)
     renames Style_Inst.Check_Xtra_Parens;
   --  Called after scanning an if, case or quantified expression that has at
   --  least one level of parentheses around the entire expression.

   function Mode_In_Check return Boolean
     renames Style_Inst.Mode_In_Check;
   --  Determines whether style checking is active and the Mode_In_Check is
   --  set, forbidding the explicit use of mode IN.

   procedure No_End_Name (Name : Node_Id)
     renames Style_Inst.No_End_Name;
   --  Called if an END is encountered where a name is allowed but not present.
   --  The parameter is the node whose name is the name that is permitted in
   --  the END line, and the scan pointer is positioned so that if an error
   --  message is to be generated in this situation, it should be generated
   --  using Error_Msg_SP.

   procedure No_Exit_Name (Name : Node_Id)
     renames Style_Inst.No_Exit_Name;
   --  Called when exiting a named loop, but a name is not present on the EXIT.
   --  The parameter is the node whose name should have followed EXIT, and the
   --  scan pointer is positioned so that if an error message is to be
   --  generated, it should be generated using Error_Msg_SP.

   procedure Non_Lower_Case_Keyword
     renames Style_Inst.Non_Lower_Case_Keyword;
   --  Called if a reserved keyword is scanned which is not spelled in all
   --  lower case letters. On entry Token_Ptr points to the keyword token.
   --  This is not used for keywords appearing as attribute designators,
   --  where instead Check_Attribute_Name (True) is called.

end Style;
