GNAT Coding Style: A Guide for GNAT Developers
==============================================

General
-------

Most of GNAT is written in Ada using a consistent style to ensure
readability of the code.  This document has been written to help
maintain this consistent style, while having a large group of developers
work on the compiler.

For the coding style in the C parts of the compiler and run time,
see the GNU Coding Guidelines.

This document is structured after the Ada Reference Manual.
Those familiar with that document should be able to quickly
lookup style rules for particular constructs.

Lexical Elements
----------------

Character Set and Separators
****************************

.. index:: Character set
.. index:: ASCII
.. index:: Separators
.. index:: End-of-line
.. index:: Line length
.. index:: Indentation

* The character set used should be plain 7-bit ASCII.
  The only separators allowed are space and the end-of-line sequence.
  No other control character or format effector (such as ``HT``,
  ``VT``, ``FF`` )
  should be used.
  The normal end-of-line sequence is used, which may be
  ``LF``, ``CR/LF`` or ``CR``,
  depending on the host system.  An optional ``SUB``
  ( ``16#1A#`` ) may be present as the
  last character in the file on hosts using that character as file terminator.

* Files that are checked in or distributed should be in host format.

* A line should never be longer than 79 characters, not counting the line
  separator.

* Lines must not have trailing blanks.

* Indentation is 3 characters per level for ``if`` statements, loops, and
  ``case`` statements.
  For exact information on required spacing between lexical
  elements, see file style.adb.

  .. index:: style.adb file

Identifiers
***********

* Identifiers will start with an upper case letter, and each letter following
  an underscore will be upper case.

  .. index:: Casing (for identifiers)

  Short acronyms may be all upper case.
  All other letters are lower case.
  An exception is for identifiers matching a foreign language.  In particular,
  we use all lower case where appropriate for C.

* Use underscores to separate words in an identifier.

  .. index:: Underscores

* Try to limit your use of abbreviations in identifiers.
  It is ok to make a few abbreviations, explain what they mean, and then
  use them frequently, but don't use lots of obscure abbreviations.  An
  example is the ``ALI`` word which stands for Ada Library
  Information and is by convention always written in upper-case when
  used in entity names.

  .. code-block:: ada

           procedure Find_ALI_Files;

* Don't use the variable name ``I``, use ``J`` instead; ``I`` is too
  easily confused with ``1`` in some fonts.  Similarly don't use the
  variable ``O``, which is too easily mistaken for the number ``0``.

Numeric Literals
****************

* Numeric literals should include underscores where helpful for
  readability.

  .. index:: Underscores

  .. code-block:: ada

          1_000_000
          16#8000_0000#
          3.14159_26535_89793_23846

Reserved Words
**************

* Reserved words use all lower case.

  .. index:: Casing (for reserved words)

  .. code-block:: ada

           return else

* The words ``Access``, ``Delta`` and ``Digits`` are
  capitalized when used as attribute_designator.

Comments
********

* A comment starts with ``--`` followed by two spaces.
  The only exception to this rule (i.e. one space is tolerated) is when the
  comment ends with a single space followed by ``--``.
  It is also acceptable to have only one space between ``--`` and the start
  of the comment when the comment is at the end of a line,
  after some Ada code.

* Every sentence in a comment should start with an upper-case letter (including
  the first letter of the comment).

  .. index:: Casing (in comments)

* When declarations are commented with 'hanging' comments, i.e.
  comments after the declaration, there is no blank line before the
  comment, and if it is absolutely necessary to have blank lines within
  the comments, e.g. to make paragraph separations within a single comment,
  these blank lines *do* have a ``--`` (unlike the
  normal rule, which is to use entirely blank lines for separating
  comment paragraphs).  The comment starts at same level of indentation
  as code it is commenting.

  .. index:: Blank lines (in comments)
  .. index:: Indentation

  .. code-block:: ada

           z : Integer;
           --  Integer value for storing value of z
           --
           --  The previous line was a blank line.

* Comments that are dubious or incomplete, or that comment on possibly
  wrong or incomplete code, should be preceded or followed by ``???``.

* Comments in a subprogram body must generally be surrounded by blank lines.
  An exception is a comment that follows a line containing a single keyword
  ( ``begin``, ``else``, ``loop`` ):

  .. code-block:: ada

           begin
              --  Comment for the next statement

              A := 5;

              --  Comment for the B statement

              B := 6;
           end;

* In sequences of statements, comments at the end of the lines should be
  aligned.

  .. index:: Alignment (in comments)

  .. code-block:: ada

            My_Identifier := 5;      --  First comment
            Other_Id := 6;           --  Second comment

* Short comments that fit on a single line are *not* ended with a
  period.  Comments taking more than a line are punctuated in the normal
  manner.

* Comments should focus on *why* instead of *what*.
  Descriptions of what subprograms do go with the specification.

* Comments describing a subprogram spec should specifically mention the
  formal argument names.  General rule: write a comment that does not
  depend on the names of things.  The names are supplementary, not
  sufficient, as comments.

* *Do not* put two spaces after periods in comments.

Declarations and Types
----------------------

* In entity declarations, colons must be surrounded by spaces.  Colons
  should be aligned.

  .. index:: Alignment (in declarations)

  .. code-block:: ada

            Entity1   : Integer;
            My_Entity : Integer;

* Declarations should be grouped in a logical order.
  Related groups of declarations may be preceded by a header comment.

* All local subprograms in a subprogram or package body should be declared
  before the first local subprogram body.

* Do not declare local entities that hide global entities.

  .. index:: Hiding of outer entities

* Do not declare multiple variables in one declaration that spans lines.
  Start a new declaration on each line, instead.

* The defining_identifiers of global declarations serve as
  comments of a sort.  So don't choose terse names, but look for names
  that give useful information instead.

* Local names can be shorter, because they are used only within
  one context, where comments explain their purpose.

* When starting an initialization or default expression on the line that follows
  the declaration line, use 2 characters for indentation.

  .. code-block:: ada

            Entity1 : Integer :=
              Function_Name (Parameters, For_Call);

* If an initialization or default expression needs to be continued on subsequent
  lines, the continuations should be indented from the start of the expression.

  .. code-block:: ada

            Entity1 : Integer := Long_Function_Name
                                   (parameters for call);

Expressions and Names
---------------------

* Every operator must be surrounded by spaces. An exception is that
  this rule does not apply to the exponentiation operator, for which
  there are no specific layout rules. The reason for this exception
  is that sometimes it makes clearer reading to leave out the spaces
  around exponentiation.

  .. index:: Operators

  .. code-block:: ada

           E := A * B**2 + 3 * (C - D);

* Use parentheses where they clarify the intended association of operands
  with operators:

  .. index:: Parenthesization of expressions

  .. code-block:: ada

           (A / B) * C

Statements
----------

Simple and Compound Statements
******************************

* Use only one statement or label per line.

* A longer sequence_of_statements may be divided in logical
  groups or separated from surrounding code using a blank line.


If Statements
*************

* When the ``if``, ``elsif`` or ``else`` keywords fit on the
  same line with the condition and the ``then`` keyword, then the
  statement is formatted as follows:

  .. index:: Alignment (in an if statement)

  .. code-block:: ada

            if condition then
               ...
            elsif condition then
               ...
            else
               ...
            end if;

  When the above layout is not possible, ``then`` should be aligned
  with ``if``, and conditions should preferably be split before an
  ``and`` or ``or`` keyword a follows:

  .. code-block:: ada

            if long_condition_that_has_to_be_split
              and then continued_on_the_next_line
            then
               ...
            end if;

  The ``elsif``, ``else`` and ``end if`` always line up with
  the ``if`` keyword.  The preferred location for splitting the line
  is before ``and`` or ``or``.  The continuation of a condition is
  indented with two spaces or as many as needed to make nesting clear.
  As an exception, if conditions are closely related either of the
  following is allowed:

  .. code-block:: ada

         if x = lakdsjfhlkashfdlkflkdsalkhfsalkdhflkjdsahf
              or else
            x = asldkjhalkdsjfhhfd
              or else
            x = asdfadsfadsf
         then
           ...
         end if;

         if x = lakdsjfhlkashfdlkflkdsalkhfsalkdhflkjdsahf or else
            x = asldkjhalkdsjfhhfd                         or else
            x = asdfadsfadsf
         then
           ...
         end if;

* Conditions should use short-circuit forms ( ``and then``,
  ``or else`` ), except when the operands are boolean variables
  or boolean constants.

  .. index:: Short-circuit forms

* Complex conditions in ``if`` statements are indented two characters:

  .. index:: Indentation (in if statements)

  .. code-block:: ada

          if this_complex_condition
            and then that_other_one
            and then one_last_one
          then
             ...
          end if;

  There are some cases where complex conditionals can be laid out
  in manners that do not follow these rules to preserve better
  parallelism between branches, e.g.

  .. code-block:: ada

          if xyz.abc (gef) = 'c'
               or else
             xyz.abc (gef) = 'x'
          then
             ...
          end if;

* Every ``if`` block is preceded and followed by a blank line, except
  where it begins or ends a sequence_of_statements.

  .. index:: Blank lines (in an if statement)

  .. code-block:: ada

            A := 5;

            if A = 5 then
               null;
            end if;

            A := 6;

Case Statements
***************

* Layout is as below.  For long ``case`` statements, the extra indentation
  can be saved by aligning the ``when`` clauses with the opening ``case``.

  .. code-block:: ada

           case expression is
              when condition =>
                 ...
              when condition =>
                 ...
           end case;

Loop Statements
***************

* When possible, have ``for`` or ``while`` on one line with the
  condition and the ``loop`` keyword.

  .. code-block:: ada

           for J in S'Range loop
              ...
           end loop;

  If the condition is too long, split the condition (see 'If
  statements' above) and align ``loop`` with the ``for`` or
  ``while`` keyword.

  .. index:: Alignment (in a loop statement)

  .. code-block:: ada

          while long_condition_that_has_to_be_split
            and then continued_on_the_next_line
          loop
             ...
          end loop;

  If the loop_statement has an identifier, it is laid out as follows:

  .. code-block:: ada

          Outer : while not condition loop
             ...
          end Outer;

Block Statements
****************

* The ``declare`` (optional), ``begin`` and ``end`` words
  are aligned, except when the block_statement is named.  There
  is a blank line before the ``begin`` keyword:

  .. index:: Alignment (in a block statement)

  .. code-block:: ada

          Some_Block : declare
             ...

          begin
             ...
          end Some_Block;

Subprograms
-----------

Subprogram Declarations
***********************

* Do not write the ``in`` for parameters.

  .. code-block:: ada

          function Length (S : String) return Integer;

* When the declaration line for a procedure or a function is too long to fit
  the entire declaration (including the keyword procedure or function) on a
  single line, then fold it, putting a single parameter on a line, aligning
  the colons, as in:

  .. code-block:: ada

         procedure Set_Heading
           (Source : String;
            Count  : Natural;
            Pad    : Character := Space;
            Fill   : Boolean   := True);

  In the case of a function, if the entire spec does not fit on one line, then
  the return may appear after the last parameter, as in:

  .. code-block:: ada

          function Head
            (Source : String;
             Count  : Natural;
             Pad    : Character := Space) return String;

  Or it may appear on its own as a separate line. This form is preferred when
  putting the return on the same line as the last parameter would result in
  an overlong line. The return type may optionally be aligned with the types
  of the parameters (usually we do this aligning if it results only in a small
  number of extra spaces, and otherwise we don't attempt to align). So two
  alternative forms for the above spec are:

  .. code-block:: ada

          function Head
            (Source : String;
             Count  : Natural;
             Pad    : Character := Space)
             return   String;

          function Head
            (Source : String;
             Count  : Natural;
             Pad    : Character := Space)
             return String;

Subprogram Bodies
*****************

* Function and procedure bodies should usually be sorted alphabetically. Do
  not attempt to sort them in some logical order by functionality. For a
  sequence of subprogram specs, a general alphabetical sorting is also
  usually appropriate, but occasionally it makes sense to group by major
  function, with appropriate headers.

* All subprograms have a header giving the function name, with the following
  format:

  .. code-block:: ada

          -----------------
          -- My_Function --
          -----------------

          procedure My_Function is
          begin
            ...
          end My_Function;

  Note that the name in the header is preceded by a single space,
  not two spaces as for other comments. These headers are used on
  nested subprograms as well as outer level subprograms. They may
  also be used as headers for sections of comments, or collections
  of declarations that are related.

* Every subprogram body must have a preceding subprogram_declaration,
  which includes proper client documentation so that you do not need to
  read the subprogram body in order to understand what the subprogram does and
  how to call it. All subprograms should be documented, without exceptions.

  .. index:: Blank lines (in subprogram bodies)

* A sequence of declarations may optionally be separated from the following
  begin by a blank line.  Just as we optionally allow blank lines in general
  between declarations, this blank line should be present only if it improves
  readability. Generally we avoid this blank line if the declarative part is
  small (one or two lines) and the body has no blank lines, and we include it
  if the declarative part is long or if the body has blank lines.

* If the declarations in a subprogram contain at least one nested
  subprogram body, then just before the ``begin`` of the enclosing
  subprogram, there is a comment line and a blank line:

  .. code-block:: ada

        --  Start of processing for Enclosing_Subprogram

        begin
          ...
        end Enclosing_Subprogram;

* When nested subprograms are present, variables that are referenced by any
  nested subprogram should precede the nested subprogram specs. For variables
  that are not referenced by nested procedures, the declarations can either also
  be before any of the nested subprogram specs (this is the old style, more
  generally used). Or then can come just before the begin, with a header. The
  following example shows the two possible styles:

  .. code-block:: ada

        procedure Style1 is
           Var_Referenced_In_Nested      : Integer;
           Var_Referenced_Only_In_Style1 : Integer;

           proc Nested;
           --  Comments ...

           ------------
           -- Nested --
           ------------

           procedure Nested is
           begin
              ...
           end Nested;

        --  Start of processing for Style1

        begin
           ...
        end Style1;

        procedure Style2 is
           Var_Referenced_In_Nested : Integer;

           proc Nested;
           --  Comments ...

           ------------
           -- Nested --
           ------------

           procedure Nested is
           begin
              ...
           end Nested;

           --  Local variables

           Var_Referenced_Only_In_Style2 : Integer;

        --  Start of processing for Style2

        begin
           ...
        end Style2;

  For new code, we generally prefer Style2, but we do not insist on
  modifying all legacy occurrences of Style1, which is still much
  more common in the sources.

Packages and Visibility Rules
-----------------------------

* All program units and subprograms have their name at the end:

  .. code-block:: ada

          package P is
             ...
          end P;

* We will use the style of ``use`` -ing ``with`` -ed packages, with
  the context clauses looking like:

  .. index:: use clauses

  .. code-block:: ada

          with A; use A;
          with B; use B;

* Names declared in the visible part of packages should be
  unique, to prevent name clashes when the packages are ``use`` d.

  .. index:: Name clash avoidance

  .. code-block:: ada

          package Entity is
             type Entity_Kind is ...;
             ...
          end Entity;

* After the file header comment, the context clause and unit specification
  should be the first thing in a program_unit.

* Preelaborate, Pure and Elaborate_Body pragmas should be added right after the
  package name, indented an extra level and using the parameterless form:

  .. code-block:: ada

          package Preelaborate_Package is
             pragma Preelaborate;
             ...
          end Preelaborate_Package;

Program Structure and Compilation Issues
----------------------------------------

* Every GNAT source file must be compiled with the ``-gnatg``
  switch to check the coding style.
  (Note that you should look at
  style.adb to see the lexical rules enforced by ``-gnatg`` ).

  .. index:: -gnatg option (to gcc)
  .. index:: style.adb file

* Each source file should contain only one compilation unit.

* Filenames should be 8 or fewer characters, followed by the ``.adb``
  extension for a body or ``.ads`` for a spec.

  .. index:: File name length

* Unit names should be distinct when 'krunch'ed to 8 characters
  (see krunch.ads) and the filenames should match the unit name,
  except that they are all lower case.

  .. index:: krunch.ads file

.. toctree::
   gnu_free_documentation_license
