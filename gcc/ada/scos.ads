------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 S C O S                                  --
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

--  This package defines tables used to store Source Coverage Obligations. It
--  is used by Par_SCO to build the SCO information before writing it out to
--  the ALI file, and by Get_SCO/Put_SCO to read and write the text form that
--  is used in the ALI file.

with Types; use Types;

with GNAT.Table;

package SCOs is

   --  SCO information can exist in one of two forms. In the ALI file, it is
   --  represented using a text format that is described in this specification.
   --  Internally it is stored using two tables SCO_Table and SCO_Unit_Table,
   --  which are also defined in this unit.

   --  Par_SCO is part of the compiler. It scans the parsed source tree and
   --  populates the internal tables.

   --  Get_SCO reads the text lines in ALI format and populates the internal
   --  tables with corresponding information.

   --  Put_SCO reads the internal tables and generates text lines in the ALI
   --  format.

   --------------------
   -- SCO ALI Format --
   --------------------

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
   --    expression in any other context, for example, on the right side of an
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
   --      expression ::= & term term      (if expr is AND or AND THEN)
   --      expression ::= | term term      (if expr is OR or OR ELSE)
   --      expression ::= ^ term term      (if expr is XOR)
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

   --    & indicates either AND or AND THEN connecting two conditions. In the
   --    context of couverture we only permit AND THEN in the source in any
   --    case, so & can always be understood to be AND THEN.

   --    | indicates either OR or OR ELSE connection two conditions. In the
   --    context of couverture we only permit OR ELSE in the source in any
   --    case, so | can always be understood to be OR ELSE.

   --    ^ indicates XOR connecting two conditions. In the context of
   --    couverture, we do not permit XOR, so this will never appear.

   --    ! indicates NOT applied to the expression.

   ---------------------------------------------------------------------
   -- Internal table used to store Source Coverage Obligations (SCOs) --
   ---------------------------------------------------------------------

   type Source_Location is record
      Line : Logical_Line_Number;
      Col  : Column_Number;
   end record;

   No_Source_Location : Source_Location := (No_Line_Number, No_Column_Number);

   type SCO_Table_Entry is record
      From : Source_Location;
      To   : Source_Location;
      C1   : Character;
      C2   : Character;
      Last : Boolean;
   end record;

   package SCO_Table is new GNAT.Table (
     Table_Component_Type => SCO_Table_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 500,
     Table_Increment      => 300);

   --  The SCO_Table_Entry values appear as follows:

   --    Statements
   --      C1   = 'S'
   --      C2   = ' '
   --      From = starting source location
   --      To   = ending source location
   --      Last = unused

   --    Exit
   --      C1   = 'T'
   --      C2   = ' '
   --      From = starting source location
   --      To   = ending source location
   --      Last = unused

   --    Simple Decision
   --      C1   = 'I', 'E', 'W', 'X' (if/exit/while/expression)
   --      C2   = 'c', 't', or 'f'
   --      From = starting source location
   --      To   = ending source location
   --      Last = True

   --    Complex Decision
   --      C1   = 'I', 'E', 'W', 'X' (if/exit/while/expression)
   --      C2   = ' '
   --      From = No_Location
   --      To   = No_Location
   --      Last = False

   --    Operator
   --      C1   = '!', '^', '&', '|'
   --      C2   = ' '
   --      From = No_Location
   --      To   = No_Location
   --      Last = False

   --    Element
   --      C1   = ' '
   --      C2   = 'c', 't', or 'f' (condition/true/false)
   --      From = starting source location
   --      To   = ending source location
   --      Last = False for all but the last entry, True for last entry

   --    Note: the sequence starting with a decision, and continuing with
   --    operators and elements up to and including the first one labeled with
   --    Last=True, indicate the sequence to be output for a complex decision
   --    on a single CD decision line.

   ----------------
   -- Unit Table --
   ----------------

   --  This table keeps track of the units and the corresponding starting and
   --  ending indexes (From, To) in the SCO table. Note that entry zero is
   --  unused, it is for convenience in calling the sort routine. Thus the
   --  real lower bound for active entries is 1.

   type SCO_Unit_Index is new Int;
   --  Used to index values in this table. Values start at 1 and are assigned
   --  sequentially as entries are constructed.

   type SCO_Unit_Table_Entry is record
      File_Name : String_Ptr;
      --  Pointer to file name in ALI file

      Dep_Num : Nat;
      --  Dependency number in ALI file

      From : Nat;
      --  Starting index in SCO_Table of SCO information for this unit

      To : Nat;
      --  Ending index in SCO_Table of SCO information for this unit
   end record;

   package SCO_Unit_Table is new GNAT.Table (
     Table_Component_Type => SCO_Unit_Table_Entry,
     Table_Index_Type     => SCO_Unit_Index,
     Table_Low_Bound      => 0, -- see note above on sorting
     Table_Initial        => 20,
     Table_Increment      => 200);

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Reset tables for a new compilation

   procedure Add_SCO
     (From : Source_Location := No_Source_Location;
      To   : Source_Location := No_Source_Location;
      C1   : Character       := ' ';
      C2   : Character       := ' ';
      Last : Boolean         := False);
   --  Adds one entry to SCO table with given field values

end SCOs;
