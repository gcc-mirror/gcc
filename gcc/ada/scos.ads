------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 S C O S                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2009-2021, Free Software Foundation, Inc.         --
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

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file scos.h

with Namet; use Namet;
with Table;
with Types; use Types;

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
   --  lines are generated if the -gnateS switch is set.

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

   --      C dependency-number filename

   --        This header precedes SCO information for the unit identified by
   --        dependency number and file name. The dependency number is the
   --        index into the generated D lines and is ones origin (i.e. 2 =
   --        reference to second generated D line).

   --        Note that the filename here will reflect the original name if
   --        a Source_Reference pragma was encountered (since all line number
   --        references will be with respect to the original file).

   --        Note: the filename is redundant in that it could be deduced from
   --        the corresponding D line, but it is convenient at least for human
   --        reading of the SCO information, and means that the SCO information
   --        can stand on its own without needing other parts of the ALI file.

   --  Statements

   --    For the purpose of SCO generation, the notion of statement includes
   --    simple statements and also the following declaration types:

   --      type_declaration
   --      subtype_declaration
   --      object_declaration
   --      renaming_declaration
   --      generic_instantiation

   --    and the following regions of the syntax tree:

   --      the part of a case_statement from CASE up to the expression
   --      the part of a FOR loop iteration scheme from FOR up to the
   --        loop_parameter_specification
   --      the part of a WHILE loop up to the condition
   --      the part of an extended_return_statement from RETURN up to the
   --        expression (if present) or to the return_subtype_indication (if
   --        no expression)

   --    and any pragma that occurs at a place where a statement or declaration
   --    is allowed.

   --  Statement lines

   --    These lines correspond to one or more successive statements (in the
   --    sense of the above list) which are always executed in sequence (in the
   --    absence of exceptions or other external interruptions).

   --    Entry points to such sequences are:

   --      the first declaration of any declarative_part
   --      the first statement of any sequence_of_statements that is not in a
   --        body or block statement that has a non-empty declarative part
   --      the first statement after a compound statement
   --      the first statement after an EXIT, RAISE or GOTO statement
   --      any statement with a label (the label itself is not part of the
   --       entry point that is recorded).

   --    Each entry point must appear as the first statement entry on a CS
   --    line. Thus, if any simple statement on a CS line is known to have
   --    been executed, then all statements that appear before it on the same
   --    CS line are certain to also have been executed.

   --    The form of a statement line in the ALI file is:

   --      CS [dominance] *sloc-range [*sloc-range...]

   --    where each sloc-range corresponds to a single statement, and * is
   --    one of:

   --      t        type declaration
   --      s        subtype declaration
   --      o        object declaration
   --      r        renaming declaration
   --      i        generic instantiation
   --      d        any other kind of declaration
   --      A        ACCEPT statement (from ACCEPT to end of parameter profile)
   --      C        CASE statement (from CASE to end of expression)
   --      E        EXIT statement
   --      F        FOR loop (from FOR to end of iteration scheme)
   --      I        IF statement (from IF to end of condition)
   --      P[name:] PRAGMA with the indicated name
   --      p[name:] disabled PRAGMA with the indicated name
   --      R        extended RETURN statement
   --      S        SELECT statement
   --      W        WHILE loop statement (from WHILE to end of condition)
   --      X        body of a degenerate subprogram (null procedure or
   --               expression function)

   --      Note: for I and W, condition above is in the RM syntax sense (this
   --      condition is a decision in SCO terminology).

   --    and is omitted for all other cases

   --    The optional dominance marker is of the form gives additional
   --    information as to how the sequence of statements denoted by the CS
   --    line can be entered:

   --      >F<sloc>
   --        sequence is entered only if the decision at <sloc> is False
   --      >T<sloc>
   --        sequence is entered only if the decision at <sloc> is True

   --      >S<sloc>
   --        sequence is entered only if the statement at <sloc> has been
   --        executed

   --      >E<sloc-range>
   --        sequence is the sequence of statements for a exception_handler
   --        with the given sloc range

   --    Note: up to 6 entries can appear on a single CS line. If more than 6
   --    entries appear in one logical statement sequence, continuation lines
   --    are marked by Cs and appear immediately after the CS line.

   --    Implementation permission: a SCO generator is permitted to emit a
   --    narrower SLOC range for a statement if the corresponding code
   --    generation circuitry ensures that all debug information for the code
   --    implementing the statement will be labeled with SLOCs that fall within
   --    that narrower range.

   --  Decisions

   --    Note: in the following description, logical operator includes only the
   --    short-circuited forms and NOT (so can be only NOT, AND THEN, OR ELSE).
   --    The reason that we can exclude AND/OR/XOR is that we expect SCO's to
   --    be generated using the restriction No_Direct_Boolean_Operators if we
   --    are interested in decision coverage, which does not permit the use of
   --    AND/OR/XOR on boolean operands. These are permitted on modular integer
   --    types, but such operations do not count as decisions in any case. If
   --    we are generating SCO's only for simple coverage, then we are not
   --    interested in decisions in any case.

   --    Note: the reason we include NOT is for informational purposes. The
   --    presence of NOT does not generate additional coverage obligations,
   --    but if we know where the NOT's are, the coverage tool can generate
   --    more accurate diagnostics on uncovered tests.

   --    A top level boolean expression is a boolean expression that is not an
   --    operand of a logical operator.

   --    Decisions are either simple or complex. A simple decision is a top
   --    level boolean expression that has only one condition and that occurs
   --    in the context of a control structure in the source program, including
   --    WHILE, IF, EXIT WHEN, or immediately within an Assert, Check,
   --    Pre_Condition or Post_Condition pragma, or as the first argument of a
   --    dyadic pragma Debug. Note that a top level boolean expression with
   --    only one condition that occurs in any other context, for example as
   --    right hand side of an assignment, is not considered to be a (simple)
   --    decision.

   --    A complex decision is a top level boolean expression that has more
   --    than one condition. A complex decision may occur in any boolean
   --    expression context.

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

   --      C* sloc expression

   --    Here * is one of the following:

   --      E       decision in EXIT WHEN statement
   --      G       decision in entry guard
   --      I       decision in IF statement or if expression
   --      P       decision in pragma Assert / Check / Pre/Post_Condition
   --      A[name] decision in aspect Pre/Post (aspect name optional)
   --      W       decision in WHILE iteration scheme
   --      X       decision in some other expression context

   --    For E, G, I, P, W, sloc is the source location of the EXIT, ENTRY, IF,
   --    PRAGMA or WHILE token, respectively

   --    For A sloc is the source location of the aspect identifier

   --    For X, sloc is omitted

   --    The expression is a prefix polish form indicating the structure of
   --    the decision, including logical operators and short-circuit forms.
   --    The following is a grammar showing the structure of expression:

   --      expression ::= term             (if expr is not logical operator)
   --      expression ::= &sloc term term  (if expr is AND or AND THEN)
   --      expression ::= |sloc term term  (if expr is OR or OR ELSE)
   --      expression ::= !sloc term       (if expr is NOT)

   --      In the last three cases, sloc is the source location of the AND, OR,
   --      or NOT token, respectively.

   --      term ::= element
   --      term ::= expression

   --      element ::= *sloc-range

   --    where * is one of the following letters:

   --      c  condition
   --      t  true condition
   --      f  false condition

   --      t/f are used to mark a condition that has been recognized by the
   --      compiler as always being true or false. c is the normal case of
   --      conditions whose value is not known at compile time.

   --    & indicates AND THEN connecting two conditions

   --    | indicates OR ELSE connecting two conditions

   --    ! indicates NOT applied to the expression

   --    Note that complex decisions do NOT include non-short-circuited logical
   --    operators (AND/XOR/OR). In the context of existing coverage tools the
   --    No_Direct_Boolean_Operators restriction is assumed, so these operators
   --    cannot appear in the source in any case.

   --    The SCO line for a decision always occurs after the CS line for the
   --    enclosing statement. The SCO line for a nested decision always occurs
   --    after the line for the enclosing decision.

   --    Note that membership tests are considered to be a single simple
   --    condition, and that is true even if the Ada 2005 set membership
   --    form is used, e.g. A in (2,7,11.15).

   --    Implementation permission: a SCO generator is permitted to emit a
   --    narrower SLOC range for a condition if the corresponding code
   --    generation circuitry ensures that all debug information for the code
   --    evaluating the condition will be labeled with SLOCs that fall within
   --    that narrower range.

   --  Case Expressions

   --    For case statements, we rely on statement coverage to make sure that
   --    all branches of a case statement are covered, but that does not work
   --    for case expressions, since the entire expression is contained in a
   --    single statement. However, for complete coverage we really should be
   --    able to check that every branch of the case statement is covered, so
   --    we generate a SCO of the form:

   --      CC sloc-range sloc-range ...

   --    where sloc-range covers the range of the case expression

   --    Note: up to 6 entries can appear on a single CC line. If more than 6
   --    entries appear in one logical statement sequence, continuation lines
   --    are marked by Cc and appear immediately after the CC line.

   --  Generic instances

   --    A table of all generic instantiations in the compilation is generated
   --    whose entries have the form:

   --      C i index dependency-number|sloc [enclosing]

   --    Where index is the 1-based index of the entry in the table,
   --    dependency-number and sloc indicate the source location of the
   --    instantiation, and enclosing is the index of the enclosing
   --    instantiation in the table (for a nested instantiation), or is
   --    omitted for an outer instantiation.

   --  Disabled pragmas

   --    No SCO is generated for disabled pragmas

   ---------------------------------------------------------------------
   -- Internal table used to store Source Coverage Obligations (SCOs) --
   ---------------------------------------------------------------------

   type Source_Location is record
      Line : Logical_Line_Number;
      Col  : Column_Number;
   end record;

   No_Source_Location : constant Source_Location :=
                          (No_Line_Number, No_Column_Number);

   type SCO_Table_Entry is record
      From : Source_Location := No_Source_Location;
      To   : Source_Location := No_Source_Location;
      C1   : Character       := ' ';
      C2   : Character       := ' ';
      Last : Boolean         := False;

      Pragma_Sloc : Source_Ptr := No_Location;
      --  For the decision SCO of a pragma, or for the decision SCO of any
      --  expression nested in a pragma Debug/Assert/PPC, location of PRAGMA
      --  token (used for control of SCO output, value not recorded in ALI
      --  file). Similarly, for the decision SCO of an aspect, or for the
      --  decision SCO of any expression nested in an aspect, location of
      --  aspect identifier token.

      Pragma_Aspect_Name : Name_Id := No_Name;
      --  For the SCO for a pragma/aspect, gives the pragma/apsect name
   end record;

   package SCO_Table is new Table.Table (
     Table_Component_Type => SCO_Table_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 500,
     Table_Increment      => 300,
     Table_Name           => "Table");

   Is_Decision : constant array (Character) of Boolean :=
     ('E' | 'G' | 'I' | 'P' | 'a' | 'A' | 'W' | 'X' => True,
      others                                        => False);
   --  Indicates which C1 values correspond to decisions

   --  The SCO_Table_Entry values appear as follows:

   --    Statements
   --      C1   = 'S'
   --      C2   = statement type code to appear on CS line (or ' ' if none)
   --      From = starting source location
   --      To   = ending source location
   --      Last = False for all but the last entry, True for last entry

   --    Note: successive statements (possibly interspersed with entries of
   --    other kinds, that are ignored for this purpose), starting with one
   --    labeled with C1 = 'S', up to and including the first one labeled with
   --    Last = True, indicate the sequence to be output for a sequence of
   --    statements on a single CS line (possibly followed by Cs continuation
   --    lines).

   --    Note: for a pragma that may be disabled (Debug, Assert, PPC, Check),
   --    the entry is initially created with C2 = 'p', to mark it as disabled.
   --    Later on during semantic analysis, if the pragma is enabled,
   --    Set_SCO_Pragma_Enabled changes C2 to 'P' to cause the entry to be
   --    emitted in Put_SCOs.

   --    Dominance marker
   --      C1   = '>'
   --      C2   = 'F'/'T'/'S'/'E'
   --      From = Decision/statement sloc ('F'/'T'/'S'),
   --             handler first sloc ('E')
   --      To   = No_Source_Location ('F'/'T'/'S'), handler last sloc ('E')

   --    Note: A dominance marker is always followed by a statement entry

   --    Decision (EXIT/entry guard/IF/WHILE)
   --      C1   = 'E'/'G'/'I'/'W' (for EXIT/entry Guard/IF/WHILE)
   --      C2   = ' '
   --      From = EXIT/ENTRY/IF/WHILE token
   --      To   = No_Source_Location
   --      Last = unused

   --    Decision (PRAGMA)
   --      C1   = 'P'
   --      C2   = ' '
   --      From = PRAGMA token
   --      To   = No_Source_Location
   --      Last = unused

   --    Note: when the parse tree is first scanned, we unconditionally build a
   --    pragma decision entry for any decision in a pragma (here as always in
   --    SCO contexts, the only pragmas with decisions are Assert, Check,
   --    dyadic Debug, Precondition and Postcondition). These entries will
   --    be omitted in output if the pragma is disabled (see comments for
   --    statement entries): this filtering is achieved during the second pass
   --    of SCO generation (Par_SCO.SCO_Record_Filtered).

   --    Decision (ASPECT)
   --      C1   = 'A'
   --      C2   = ' '
   --      From = aspect identifier
   --      To   = No_Source_Location
   --      Last = unused

   --    Note: when the parse tree is first scanned, we unconditionally build a
   --    pragma decision entry for any decision in an aspect (Pre/Post/
   --    [Type_]Invariant/[Static_|Dynamic_]Predicate). Entries for disabled
   --    Pre/Post aspects will be omitted from output.

   --    Decision (Expression)
   --      C1   = 'X'
   --      C2   = ' '
   --      From = No_Source_Location
   --      To   = No_Source_Location
   --      Last = unused

   --    Operator
   --      C1   = '!', '&', '|'
   --      C2   = ' '/'?'/ (Logical operator/Putative one)
   --      From = location of NOT/AND/OR token
   --      To   = No_Source_Location
   --      Last = False

   --    Element (condition)
   --      C1   = ' '
   --      C2   = 'c', 't', or 'f' (condition/true/false)
   --      From = starting source location
   --      To   = ending source location
   --      Last = False for all but the last entry, True for last entry

   --    Note: the sequence starting with a decision, and continuing with
   --    operators and elements up to and including the first one labeled with
   --    Last = True, indicate the sequence to be output on one decision line.

   ----------------
   -- Unit Table --
   ----------------

   --  This table keeps track of the units and the corresponding starting and
   --  ending indexes (From, To) in the SCO table. Note that entry zero is
   --  present but unused, it is for convenience in calling the sort routine.
   --  Thus the lower bound for real entries is 1.

   type SCO_Unit_Index is new Int;
   --  Used to index values in this table. Values start at 1 and are assigned
   --  sequentially as entries are constructed.

   Missing_Dep_Num : constant Nat := 0;
   --  Represents a dependency number for a dependency that is ignored. SCO
   --  information consumers use this to strip units that must be kept out of
   --  the coverage analysis.

   type SCO_Unit_Table_Entry is record
      File_Name : String_Ptr;
      --  Pointer to file name in ALI file

      File_Index : Source_File_Index;
      --  Index for the source file

      Dep_Num : Nat;
      --  Dependency number in ALI file. This is a positive number when the
      --  dependency is actually available in the context, it is
      --  Missing_Dep_Num otherwise.

      From : Nat;
      --  Starting index in SCO_Table of SCO information for this unit

      To : Nat;
      --  Ending index in SCO_Table of SCO information for this unit

      --  Warning: SCOs generation (in Par_SCO) is done in two passes, which
      --  communicate through an intermediate table (Par_SCO.SCO_Raw_Table).
      --  Before the second pass executes, From and To actually reference index
      --  in the internal table: SCO_Table is empty. Then, at the end of the
      --  second pass, these indexes are updated in order to reference indexes
      --  in SCO_Table.

   end record;

   package SCO_Unit_Table is new Table.Table (
     Table_Component_Type => SCO_Unit_Table_Entry,
     Table_Index_Type     => SCO_Unit_Index,
     Table_Low_Bound      => 0, -- see note above on sorting
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "Unit_Table");

   -----------------------
   -- Generic instances --
   -----------------------

   type SCO_Instance_Index is new Nat;

   type SCO_Instance_Table_Entry is record
      Inst_Dep_Num : Nat;
      Inst_Loc     : Source_Location;
      --  File and source location of instantiation

      Enclosing_Instance : SCO_Instance_Index;
   end record;

   package SCO_Instance_Table is new Table.Table (
     Table_Component_Type => SCO_Instance_Table_Entry,
     Table_Index_Type     => SCO_Instance_Index,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "Instance_Table");

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Reset tables for a new compilation

end SCOs;
