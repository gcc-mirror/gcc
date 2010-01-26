------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R _ S C O                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
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

--  This package contains the routines used to deal with generation and output
--  of Soure Coverage Obligations (SCO's) used for coverage analysis purposes.

with Types; use Types;

package Par_SCO is

   ----------------
   -- SCO Format --
   ----------------

   --  Source coverage obligations are generated on a unit-by-unit basis in the
   --  ALI file, using lines that start with the identifying character C. These
   --  lines are generated if the -gnatC switch is set.

   --  Sloc Ranges

   --    In several places in the SCO lines, Sloc ranges appear. These are used
   --    to indicate the first and last Sloc of some construct in the tree and
   --    they have the form:

   --      line:col-line:col

   --    Note that SCO's are generated only for generic templates, not for
   --    generic instances (since only the first are part of the source). So
   --    we don't need generic instantiation stuff in these line:col items.

   --  SCO File headers

   --    The SCO information follows the cross-reference information, so it
   --    need not be read by tools like gnatbind, gnatmake etc. The SCO output
   --    is divided into sections, one section for each unit for which SCO's
   --    are generated. A SCO section has a header of the form:

   --      C  dependency-number  filename

   --        This header precedes SCO information for the unit identified by
   --        dependency number and file name. The dependency number is the
   --        index into the generated D lines and is ones origin (i.e. 2 =
   --        reference to second generated D line).

   --        Note that the filename here will reflect the original name if
   --        a Source_Reference pragma was encountered (since all line number
   --        references will be with respect to the original file).

   --  Statements

   --    For the purpose of SCO generation, the notion of statement includes
   --    simple statements and also the following declaration types:

   --      type_declaration
   --      subtype_declaration
   --      object_declaration
   --      renaming_declaration
   --      generic_instantiation

   --  Statement lines

   --    These lines correspond to a sequence of one or more statements which
   --    are always exeecuted in sequence, The first statement may be an entry
   --    point (e.g. statement after a label), and the last statement may be
   --    an exit point (e.g. an exit statement), but no other entry or exit
   --    points may occur within the sequence of statements. The idea is that
   --    the sequence can be treated as a single unit from a coverage point of
   --    view, if any of the code for the statement sequence is executed, this
   --    corresponds to coverage of the entire statement sequence. The form of
   --    a statement line in the ALI file is:

   --      CS sloc-range

   --  Exit points

   --    An exit point is a statement that causes transfer of control. Examples
   --    are exit statements, raise statements and return statements. The form
   --    of an exit point in the ALI file is:

   --      CT sloc-range

   --  Decisions

   --    Decisions represent the most significant section of the SCO lines

   --    Note: in the following description, logical operator includes the
   --    short circuited forms (so can be any of AND, OR, XOR, NOT, AND THEN,
   --    or OR ELSE).

   --    Decisions are either simple or complex. A simple decision is a boolean
   --    expresssion that occurs in the context of a control structure in the
   --    source program, including WHILE, IF, EXIT WHEN. Note that a boolean
   --    expression in any other context, e.g. on the right side of an
   --    assignment, is not considered to be a decision.

   --    A complex decision is an occurrence of a logical operator which is not
   --    itself an operand of some other logical operator. If any operand of
   --    the logical operator is itself a logical operator, this is not a
   --    separate decision, it is part of the same decision.

   --    So for example, if we have

   --        A, B, C, D : Boolean;
   --        function F (Arg : Boolean) return Boolean);
   --        ...
   --        A and then (B or else F (C and then D))

   --    There are two (complex) decisions here:

   --        1. X and then (Y or else Z)

   --           where X = A, Y = B, and Z = F (C and then D)

   --        2. C and then D

   --    For each decision, a decision line is generated with the form:

   --      C* expression

   --    Here * is one of the following characters:

   --      I  decision in IF statement or conditional expression
   --      E  decision in EXIT WHEN statement
   --      W  decision in WHILE iteration scheme
   --      X  decision appearing in some other expression context

   --    The expression is a prefix polish form indicating the structure of
   --    the decision, including logical operators and short circuit forms.
   --    The following is a grammar showing the structure of expression:

   --      expression ::= term             (if expr is not logical operator)
   --      expression ::= & term term      (if expr is AND THEN)
   --      expression ::= | term term      (if expr is OR ELSE)
   --      expression ::= !term            (if expr is NOT)

   --      term ::= element
   --      term ::= expression

   --      element ::= outcome sloc-range

   --    outcome is one of the following letters:

   --      c  condition
   --      t  true condition
   --      f  false condition

   --      where t/f are used to mark a condition that has been recognized by
   --      the compiler as always being true or false.

   --    & indicates either AND THEN connecting two conditions

   --    | indicates either OR ELSE connection two conditions

   --    ! indicates NOT applied to the expression

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize internal tables for a new compilation

   procedure SCO_Record (U : Unit_Number_Type);
   --  This procedure scans the tree for the unit identified by U, populating
   --  internal tables recording the SCO information. Note that this is done
   --  before any semantic analysis/expansion happens.

   procedure Set_SCO_Condition (First_Loc : Source_Ptr; Typ : Character);
   --  This procedure is called during semantic analysis to record a condition
   --  which has been identified as always True (Typ = 't') or always False
   --  (Typ = 'f') by the compiler. The condition is identified by the
   --  First_Sloc value in the original tree.

   procedure SCO_Output;
   --  Outputs SCO lines for all units, with appropriate section headers, for
   --  unit U in the ALI file, as recorded by previous calls to SCO_Record,
   --  possibly modified by calls to Set_SCO_Condition.

   procedure dsco;
   --  Debug routine to dump SCO table. This is a raw format dump showing
   --  exactly what the tables contain.

   procedure pscos;
   --  Debugging procedure to output contents of SCO binary tables in the
   --  format in which they appear in an ALI file.

end Par_SCO;
