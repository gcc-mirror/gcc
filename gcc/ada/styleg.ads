------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S T Y L E G                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  This generic package collects the routines used for style checking, as
--  activated by the relevant command line option. These are gathered in
--  a separate package so that they can more easily be customized. Calls
--  to these subprograms are only made if Opt.Style_Check is set True.
--  Styleg does not depends on the GNAT tree (Atree, Sinfo, ...).

with Types; use Types;

generic
   with procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr);
   --  Output a message at specified location

   with procedure Error_Msg_S (Msg : String);
   --  Output a message at current scan pointer location

   with procedure Error_Msg_SC (Msg : String);
   --  Output a message at the start of the current token

   with procedure Error_Msg_SP (Msg : String);
   --  Output a message at the start of the previous token

package Styleg is

   procedure Check_Abs_Not;
   --  Called after scanning an ABS or NOT operator to check spacing

   procedure Check_Apostrophe;
   --  Called after scanning an apostrophe to check spacing

   procedure Check_Arrow (Inside_Depends : Boolean := False);
   --  Called after scanning out an arrow to check spacing. Inside_Depends is
   --  True if the call is from an argument of the Depends or Refined_Depends
   --  aspect or pragma (where the allowed/required format is =>+).

   procedure Check_Attribute_Name (Reserved : Boolean);
   --  The current token is an attribute designator. Check that it
   --  is capitalized in an appropriate manner. Reserved is set if
   --  the attribute designator is a reserved word (access, digits,
   --  delta or range) to allow differing rules for the two cases.

   procedure Check_Boolean_Operator (Node : Node_Id);
   --  Node is a node for an AND or OR operator. Check that the usage meets
   --  the style rules.

   procedure Check_Box;
   --  Called after scanning out a box to check spacing

   procedure Check_Binary_Operator;
   --  Called after scanning out a binary operator other than a plus, minus
   --  or exponentiation operator. Intended for checking spacing rules.

   procedure Check_Exponentiation_Operator;
   --  Called after scanning out an exponentiation operator. Intended for
   --  checking spacing rules.

   procedure Check_Colon;
   --  Called after scanning out colon to check spacing

   procedure Check_Colon_Equal;
   --  Called after scanning out colon equal to check spacing

   procedure Check_Comma;
   --  Called after scanning out comma to check spacing

   procedure Check_Comment;
   --  Called with Scan_Ptr pointing to the first minus sign of a comment.
   --  Intended for checking any specific rules for comment placement/format.

   procedure Check_Defining_Identifier_Casing;
   --  The current token is an identifier that will be a defining
   --  identifier. Check that it is mixed case, if the appropriate
   --  switch is set.

   procedure Check_Dot_Dot;
   --  Called after scanning out dot dot to check spacing

   procedure Check_EOF;
   --  Called after scanning out EOF mark

   procedure Check_HT;
   --  Called with Scan_Ptr pointing to a horizontal tab character

   procedure Check_Indentation;
   --  Called at the start of a new statement or declaration, with Token_Ptr
   --  pointing to the first token of the statement or declaration. The check
   --  is that the starting column is appropriate to the indentation rules if
   --  Token_Ptr is the first token on the line.

   procedure Check_Left_Paren;
   --  Called after scanning out a left parenthesis to check spacing

   procedure Check_Line_Max_Length (Len : Nat);
   --  Called with Scan_Ptr pointing to the first line terminator character
   --  terminating the current line. Used to check for appropriate line length.
   --  The parameter Len is the length of the current line.

   procedure Check_Line_Terminator (Len : Nat);
   --  Called with Scan_Ptr pointing to the first line terminator terminating
   --  the current line, used to check for appropriate line terminator usage.
   --  The parameter Len is the length of the current line.

   procedure Check_Not_In;
   --  Called with Scan_Ptr pointing to an IN token, and Prev_Token_Ptr
   --  pointing to a NOT token. Used to check proper layout of NOT IN.

   procedure Check_Pragma_Name;
   --  The current token is a pragma identifier. Check that it is spelled
   --  properly (i.e. with an appropriate casing convention).

   procedure Check_Right_Paren;
   --  Called after scanning out a right parenthesis to check spacing

   procedure Check_Semicolon;
   --  Called after scanning out a semicolon to check spacing

   procedure Check_Then (If_Loc : Source_Ptr);
   --  Called to check that THEN and IF keywords are appropriately positioned.
   --  The parameters show the first characters of the two keywords. This
   --  procedure is called with Token_Ptr pointing to the THEN keyword.

   procedure Check_Separate_Stmt_Lines;
   pragma Inline (Check_Separate_Stmt_Lines);
   --  Called after scanning THEN (not preceded by AND) or ELSE (not preceded
   --  by OR). Used to check that no tokens follow on the same line (which
   --  would interfere with coverage testing). Handles case of THEN ABORT as
   --  an exception, as well as PRAGMA after ELSE.

   procedure Check_Unary_Plus_Or_Minus  (Inside_Depends : Boolean := False);
   --  Called after scanning a unary plus or minus to check spacing. The flag
   --  Inside_Depends is set if we are scanning within a Depends or
   --  Refined_Depends pragma or Aspect, in which case =>+ requires a
   --  following space.

   procedure Check_Vertical_Bar;
   --  Called after scanning a vertical bar to check spacing

   procedure Check_Xtra_Parens (Loc : Source_Ptr);
   --  Called after scanning an if, case, or quantified expression that has at
   --  least one level of parentheses around the entire expression.

   function Mode_In_Check return Boolean;
   pragma Inline (Mode_In_Check);
   --  Determines whether style checking is active and the Mode_In_Check is
   --  set, forbidding the explicit use of mode IN.

   procedure No_End_Name (Name : Node_Id);
   --  Called if an END is encountered where a name is allowed but not present.
   --  The parameter is the node whose name is the name that is permitted in
   --  the END line, and the scan pointer is positioned so that if an error
   --  message is to be generated in this situation, it should be generated
   --  using Error_Msg_SP.

   procedure No_Exit_Name (Name : Node_Id);
   --  Called when exiting a named loop, but a name is not present on the EXIT.
   --  The parameter is the node whose name should have followed EXIT, and the
   --  scan pointer is positioned so that if an error message is to be
   --  generated, it should be generated using Error_Msg_SP.

   procedure Non_Lower_Case_Keyword;
   --  Called if a reserved keyword is scanned which is not spelled in all
   --  lower case letters. On entry Token_Ptr points to the keyword token.
   --  This is not used for keywords appearing as attribute designators,
   --  where instead Check_Attribute_Name (True) is called.

end Styleg;
