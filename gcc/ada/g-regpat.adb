------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                          G N A T . R E G P A T                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--               Copyright (C) 1986 by University of Toronto.               --
--           Copyright (C) 1996-2002 Ada Core Technologies, Inc.            --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  This is an altered Ada 95 version of the original V8 style regular
--  expression library written in C by Henry Spencer. Apart from the
--  translation to Ada, the interface has been considerably changed to
--  use the Ada String type instead of C-style nul-terminated strings.

--  Beware that some of this code is subtly aware of the way operator
--  precedence is structured in regular expressions. Serious changes in
--  regular-expression syntax might require a total rethink.

with System.IO;               use System.IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Unchecked_Conversion;

package body GNAT.Regpat is

   MAGIC : constant Character := Character'Val (10#0234#);
   --  The first byte of the regexp internal "program" is actually
   --  this magic number; the start node begins in the second byte.
   --
   --  This is used to make sure that a regular expression was correctly
   --  compiled.

   ----------------------------
   -- Implementation details --
   ----------------------------

   --  This is essentially a linear encoding of a nondeterministic
   --  finite-state machine, also known as syntax charts or
   --  "railroad normal form" in parsing technology.

   --  Each node is an opcode plus a "next" pointer, possibly plus an
   --  operand. "Next" pointers of all nodes except BRANCH implement
   --  concatenation; a "next" pointer with a BRANCH on both ends of it
   --  is connecting two alternatives.

   --  The operand of some types of node is a literal string; for others,
   --  it is a node leading into a sub-FSM. In particular, the operand of
   --  a BRANCH node is the first node of the branch.
   --  (NB this is *not* a tree structure:  the tail of the branch connects
   --  to the thing following the set of BRANCHes).

   --  You can see the exact byte-compiled version by using the Dump
   --  subprogram. However, here are a few examples:

   --  (a|b):  1 : MAGIC
   --          2 : BRANCH  (next at  10)
   --          5 :    EXACT  (next at  18)   operand=a
   --         10 : BRANCH  (next at  18)
   --         13 :    EXACT  (next at  18)   operand=b
   --         18 : EOP  (next at 0)
   --
   --  (ab)*:  1 : MAGIC
   --          2 : CURLYX  (next at  26)  { 0, 32767}
   --          9 :    OPEN 1  (next at  13)
   --         13 :       EXACT  (next at  19)   operand=ab
   --         19 :    CLOSE 1  (next at  23)
   --         23 :    WHILEM  (next at 0)
   --         26 : NOTHING  (next at  29)
   --         29 : EOP  (next at 0)

   --  The opcodes are:

   type Opcode is

      --  Name          Operand?  Meaning

     (EOP,        -- no        End of program
      MINMOD,     -- no        Next operator is not greedy

      --  Classes of characters

      ANY,        -- no        Match any one character except newline
      SANY,       -- no        Match any character, including new line
      ANYOF,      -- class     Match any character in this class
      EXACT,      -- str       Match this string exactly
      EXACTF,     -- str       Match this string (case-folding is one)
      NOTHING,    -- no        Match empty string
      SPACE,      -- no        Match any whitespace character
      NSPACE,     -- no        Match any non-whitespace character
      DIGIT,      -- no        Match any numeric character
      NDIGIT,     -- no        Match any non-numeric character
      ALNUM,      -- no        Match any alphanumeric character
      NALNUM,     -- no        Match any non-alphanumeric character

      --  Branches

      BRANCH,     -- node      Match this alternative, or the next

      --  Simple loops (when the following node is one character in length)

      STAR,       -- node      Match this simple thing 0 or more times
      PLUS,       -- node      Match this simple thing 1 or more times
      CURLY,      -- 2num node Match this simple thing between n and m times.

      --  Complex loops

      CURLYX,     -- 2num node Match this complex thing {n,m} times
      --                       The nums are coded on two characters each.

      WHILEM,     -- no        Do curly processing and see if rest matches

      --  Matches after or before a word

      BOL,        -- no        Match "" at beginning of line
      MBOL,       -- no        Same, assuming mutiline (match after \n)
      SBOL,       -- no        Same, assuming single line (don't match at \n)
      EOL,        -- no        Match "" at end of line
      MEOL,       -- no        Same, assuming mutiline (match before \n)
      SEOL,       -- no        Same, assuming single line (don't match at \n)

      BOUND,      -- no        Match "" at any word boundary
      NBOUND,     -- no        Match "" at any word non-boundary

      --  Parenthesis groups handling

      REFF,       -- num       Match some already matched string, folded
      OPEN,       -- num       Mark this point in input as start of #n
      CLOSE);     -- num       Analogous to OPEN

   for Opcode'Size use 8;

   --  Opcode notes:

   --  BRANCH
   --    The set of branches constituting a single choice are hooked
   --    together with their "next" pointers, since precedence prevents
   --    anything being concatenated to any individual branch. The
   --    "next" pointer of the last BRANCH in a choice points to the
   --    thing following the whole choice. This is also where the
   --    final "next" pointer of each individual branch points; each
   --    branch starts with the operand node of a BRANCH node.

   --  STAR,PLUS
   --    '?', and complex '*' and '+', are implemented with CURLYX.
   --    branches. Simple cases (one character per match) are implemented with
   --    STAR and PLUS for speed and to minimize recursive plunges.

   --  OPEN,CLOSE
   --    ...are numbered at compile time.

   --  EXACT, EXACTF
   --    There are in fact two arguments, the first one is the length (minus
   --    one of the string argument), coded on one character, the second
   --    argument is the string itself, coded on length + 1 characters.

   --  A node is one char of opcode followed by two chars of "next" pointer.
   --  "Next" pointers are stored as two 8-bit pieces, high order first. The
   --  value is a positive offset from the opcode of the node containing it.
   --  An operand, if any, simply follows the node. (Note that much of the
   --  code generation knows about this implicit relationship.)

   --  Using two bytes for the "next" pointer is vast overkill for most
   --  things, but allows patterns to get big without disasters.

   -----------------------
   -- Character classes --
   -----------------------
   --  This is the implementation for character classes ([...]) in the
   --  syntax for regular expressions. Each character (0..256) has an
   --  entry into the table. This makes for a very fast matching
   --  algorithm.

   type Class_Byte is mod 256;
   type Character_Class is array (Class_Byte range 0 .. 31) of Class_Byte;

   type Bit_Conversion_Array is array (Class_Byte range 0 .. 7) of Class_Byte;
   Bit_Conversion : constant Bit_Conversion_Array :=
                      (1, 2, 4, 8, 16, 32, 64, 128);

   type Std_Class is (ANYOF_NONE,
                      ANYOF_ALNUM,   --  Alphanumeric class [a-zA-Z0-9]
                      ANYOF_NALNUM,
                      ANYOF_SPACE,   --  Space class [ \t\n\r\f]
                      ANYOF_NSPACE,
                      ANYOF_DIGIT,   --  Digit class [0-9]
                      ANYOF_NDIGIT,
                      ANYOF_ALNUMC,  --  Alphanumeric class [a-zA-Z0-9]
                      ANYOF_NALNUMC,
                      ANYOF_ALPHA,   --  Alpha class [a-zA-Z]
                      ANYOF_NALPHA,
                      ANYOF_ASCII,   --  Ascii class (7 bits) 0..127
                      ANYOF_NASCII,
                      ANYOF_CNTRL,   --  Control class
                      ANYOF_NCNTRL,
                      ANYOF_GRAPH,   --  Graphic class
                      ANYOF_NGRAPH,
                      ANYOF_LOWER,   --  Lower case class [a-z]
                      ANYOF_NLOWER,
                      ANYOF_PRINT,   --  printable class
                      ANYOF_NPRINT,
                      ANYOF_PUNCT,   --
                      ANYOF_NPUNCT,
                      ANYOF_UPPER,   --  Upper case class [A-Z]
                      ANYOF_NUPPER,
                      ANYOF_XDIGIT,  --  Hexadecimal digit
                      ANYOF_NXDIGIT
                      );

   procedure Set_In_Class
     (Bitmap : in out Character_Class;
      C      : Character);
   --  Set the entry to True for C in the class Bitmap.

   function Get_From_Class
     (Bitmap : Character_Class;
      C      : Character)
      return   Boolean;
   --  Return True if the entry is set for C in the class Bitmap.

   procedure Reset_Class (Bitmap : in out Character_Class);
   --  Clear all the entries in the class Bitmap.

   pragma Inline (Set_In_Class);
   pragma Inline (Get_From_Class);
   pragma Inline (Reset_Class);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function "=" (Left : Character; Right : Opcode) return Boolean;

   function Is_Alnum (C : Character) return Boolean;
   --  Return True if C is an alphanum character or an underscore ('_')

   function Is_Space (C : Character) return Boolean;
   --  Return True if C is a whitespace character

   function Is_Printable (C : Character) return Boolean;
   --  Return True if C is a printable character

   function Operand (P : Pointer) return Pointer;
   --  Return a pointer to the first operand of the node at P

   function String_Length
     (Program : Program_Data;
      P       : Pointer)
      return    Program_Size;
   --  Return the length of the string argument of the node at P

   function String_Operand (P : Pointer) return Pointer;
   --  Return a pointer to the string argument of the node at P

   procedure Bitmap_Operand
     (Program : Program_Data;
      P       : Pointer;
      Op      : out Character_Class);
   --  Return a pointer to the string argument of the node at P

   function Get_Next_Offset
     (Program : Program_Data;
      IP      : Pointer)
      return    Pointer;
   --  Get the offset field of a node. Used by Get_Next.

   function Get_Next
     (Program : Program_Data;
      IP      : Pointer)
      return    Pointer;
   --  Dig the next instruction pointer out of a node

   procedure Optimize (Self : in out Pattern_Matcher);
   --  Optimize a Pattern_Matcher by noting certain special cases

   function Read_Natural
     (Program : Program_Data;
      IP      : Pointer)
      return    Natural;
   --  Return the 2-byte natural coded at position IP.

   --  All of the subprograms above are tiny and should be inlined

   pragma Inline ("=");
   pragma Inline (Is_Alnum);
   pragma Inline (Is_Space);
   pragma Inline (Get_Next);
   pragma Inline (Get_Next_Offset);
   pragma Inline (Operand);
   pragma Inline (Read_Natural);
   pragma Inline (String_Length);
   pragma Inline (String_Operand);

   type Expression_Flags is record
      Has_Width,            -- Known never to match null string
      Simple,               -- Simple enough to be STAR/PLUS operand
      SP_Start  : Boolean;  -- Starts with * or +
   end record;

   Worst_Expression : constant Expression_Flags := (others => False);
   --  Worst case

   ---------
   -- "=" --
   ---------

   function "=" (Left : Character; Right : Opcode) return Boolean is
   begin
      return Character'Pos (Left) = Opcode'Pos (Right);
   end "=";

   --------------------
   -- Bitmap_Operand --
   --------------------

   procedure Bitmap_Operand
     (Program : Program_Data;
      P       : Pointer;
      Op      : out Character_Class)
   is
      function Convert is new Unchecked_Conversion
        (Program_Data, Character_Class);

   begin
      Op (0 .. 31) := Convert (Program (P + 3 .. P + 34));
   end Bitmap_Operand;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Matcher         : out Pattern_Matcher;
      Expression      : String;
      Final_Code_Size : out Program_Size;
      Flags           : Regexp_Flags := No_Flags)
   is
      --  We can't allocate space until we know how big the compiled form
      --  will be, but we can't compile it (and thus know how big it is)
      --  until we've got a place to put the code. So we cheat: we compile
      --  it twice, once with code generation turned off and size counting
      --  turned on, and once "for real".

      --  This also means that we don't allocate space until we are sure
      --  that the thing really will compile successfully, and we never
      --  have to move the code and thus invalidate pointers into it.

      --  Beware that the optimization-preparation code in here knows
      --  about some of the structure of the compiled regexp.

      PM        : Pattern_Matcher renames Matcher;
      Program   : Program_Data renames PM.Program;

      Emit_Code : constant Boolean := PM.Size > 0;
      Emit_Ptr  : Pointer := Program_First;

      Parse_Pos : Natural := Expression'First; -- Input-scan pointer
      Parse_End : Natural := Expression'Last;

      ----------------------------
      -- Subprograms for Create --
      ----------------------------

      procedure Emit (B : Character);
      --  Output the Character to the Program.
      --  If code-generation is disables, simply increments the program
      --  counter.

      function  Emit_Node (Op : Opcode) return Pointer;
      --  If code-generation is enabled, Emit_Node outputs the
      --  opcode and reserves space for a pointer to the next node.
      --  Return value is the location of new opcode, ie old Emit_Ptr.

      procedure Emit_Natural (IP : Pointer; N : Natural);
      --  Split N on two characters at position IP.

      procedure Emit_Class (Bitmap : Character_Class);
      --  Emits a character class.

      procedure Case_Emit (C : Character);
      --  Emit C, after converting is to lower-case if the regular
      --  expression is case insensitive.

      procedure Parse
        (Parenthesized : Boolean;
         Flags         : in out Expression_Flags;
         IP            : out Pointer);
      --  Parse regular expression, i.e. main body or parenthesized thing
      --  Caller must absorb opening parenthesis.

      procedure Parse_Branch
        (Flags         : in out Expression_Flags;
         First         : Boolean;
         IP            : out Pointer);
      --  Implements the concatenation operator and handles '|'
      --  First should be true if this is the first item of the alternative.

      procedure Parse_Piece
        (Expr_Flags : in out Expression_Flags; IP : out Pointer);
      --  Parse something followed by possible [*+?]

      procedure Parse_Atom
        (Expr_Flags : in out Expression_Flags; IP : out Pointer);
      --  Parse_Atom is the lowest level parse procedure.
      --  Optimization:  gobbles an entire sequence of ordinary characters
      --  so that it can turn them into a single node, which is smaller to
      --  store and faster to run. Backslashed characters are exceptions,
      --  each becoming a separate node; the code is simpler that way and
      --  it's not worth fixing.

      procedure Insert_Operator
        (Op       : Opcode;
         Operand  : Pointer;
         Greedy   : Boolean := True);
      --  Insert_Operator inserts an operator in front of an
      --  already-emitted operand and relocates the operand.
      --  This applies to PLUS and STAR.
      --  If Minmod is True, then the operator is non-greedy.

      procedure Insert_Curly_Operator
        (Op      : Opcode;
         Min     : Natural;
         Max     : Natural;
         Operand : Pointer;
         Greedy  : Boolean := True);
      --  Insert an operator for CURLY ({Min}, {Min,} or {Min,Max}).
      --  If Minmod is True, then the operator is non-greedy.

      procedure Link_Tail (P, Val : Pointer);
      --  Link_Tail sets the next-pointer at the end of a node chain

      procedure Link_Operand_Tail (P, Val : Pointer);
      --  Link_Tail on operand of first argument; nop if operandless

      function  Next_Instruction (P : Pointer) return Pointer;
      --  Dig the "next" pointer out of a node

      procedure Fail (M : in String);
      pragma No_Return (Fail);
      --  Fail with a diagnostic message, if possible

      function Is_Curly_Operator (IP : Natural) return Boolean;
      --  Return True if IP is looking at a '{' that is the beginning
      --  of a curly operator, ie it matches {\d+,?\d*}

      function Is_Mult (IP : Natural) return Boolean;
      --  Return True if C is a regexp multiplier: '+', '*' or '?'

      procedure Get_Curly_Arguments
        (IP     : Natural;
         Min    : out Natural;
         Max    : out Natural;
         Greedy : out Boolean);
      --  Parse the argument list for a curly operator.
      --  It is assumed that IP is indeed pointing at a valid operator.

      procedure Parse_Character_Class (IP : out Pointer);
      --  Parse a character class.
      --  The calling subprogram should consume the opening '[' before.

      procedure Parse_Literal (Expr_Flags : in out Expression_Flags;
                               IP : out Pointer);
      --  Parse_Literal encodes a string of characters
      --  to be matched exactly.

      function Parse_Posix_Character_Class return Std_Class;
      --  Parse a posic character class, like [:alpha:] or [:^alpha:].
      --  The called is suppoed to absorbe the opening [.

      pragma Inline (Is_Mult);
      pragma Inline (Emit_Natural);
      pragma Inline (Parse_Character_Class); --  since used only once

      ---------------
      -- Case_Emit --
      ---------------

      procedure Case_Emit (C : Character) is
      begin
         if (Flags and Case_Insensitive) /= 0 then
            Emit (To_Lower (C));

         else
            --  Dump current character

            Emit (C);
         end if;
      end Case_Emit;

      ----------
      -- Emit --
      ----------

      procedure Emit (B : Character) is
      begin
         if Emit_Code then
            Program (Emit_Ptr) := B;
         end if;

         Emit_Ptr := Emit_Ptr + 1;
      end Emit;

      ----------------
      -- Emit_Class --
      ----------------

      procedure Emit_Class (Bitmap : Character_Class) is
         subtype Program31 is Program_Data (0 .. 31);

         function Convert is new Unchecked_Conversion
           (Character_Class, Program31);

      begin
         if Emit_Code then
            Program (Emit_Ptr .. Emit_Ptr + 31) := Convert (Bitmap);
         end if;

         Emit_Ptr := Emit_Ptr + 32;
      end Emit_Class;

      ------------------
      -- Emit_Natural --
      ------------------

      procedure Emit_Natural (IP : Pointer; N : Natural) is
      begin
         if Emit_Code then
            Program (IP + 1) := Character'Val (N / 256);
            Program (IP) := Character'Val (N mod 256);
         end if;
      end Emit_Natural;

      ---------------
      -- Emit_Node --
      ---------------

      function Emit_Node (Op : Opcode) return Pointer is
         Result : constant Pointer := Emit_Ptr;

      begin
         if Emit_Code then
            Program (Emit_Ptr) := Character'Val (Opcode'Pos (Op));
            Program (Emit_Ptr + 1) := ASCII.NUL;
            Program (Emit_Ptr + 2) := ASCII.NUL;
         end if;

         Emit_Ptr := Emit_Ptr + 3;
         return Result;
      end Emit_Node;

      ----------
      -- Fail --
      ----------

      procedure Fail (M : in String) is
      begin
         raise Expression_Error;
      end Fail;

      -------------------------
      -- Get_Curly_Arguments --
      -------------------------

      procedure Get_Curly_Arguments
        (IP     : Natural;
         Min    : out Natural;
         Max    : out Natural;
         Greedy : out Boolean)
      is
         pragma Warnings (Off, IP);

         Save_Pos : Natural := Parse_Pos + 1;

      begin
         Min := 0;
         Max := Max_Curly_Repeat;

         while Expression (Parse_Pos) /= '}'
           and then Expression (Parse_Pos) /= ','
         loop
            Parse_Pos := Parse_Pos + 1;
         end loop;

         Min := Natural'Value (Expression (Save_Pos .. Parse_Pos - 1));

         if Expression (Parse_Pos) = ',' then
            Save_Pos := Parse_Pos + 1;
            while Expression (Parse_Pos) /= '}' loop
               Parse_Pos := Parse_Pos + 1;
            end loop;

            if Save_Pos /= Parse_Pos then
               Max := Natural'Value (Expression (Save_Pos .. Parse_Pos - 1));
            end if;

         else
            Max := Min;
         end if;

         if Parse_Pos < Expression'Last
           and then Expression (Parse_Pos + 1) = '?'
         then
            Greedy := False;
            Parse_Pos := Parse_Pos + 1;

         else
            Greedy := True;
         end if;
      end Get_Curly_Arguments;

      ---------------------------
      -- Insert_Curly_Operator --
      ---------------------------

      procedure Insert_Curly_Operator
        (Op      : Opcode;
         Min     : Natural;
         Max     : Natural;
         Operand : Pointer;
         Greedy  : Boolean := True)
      is
         Dest   : constant Pointer := Emit_Ptr;
         Old    : Pointer;
         Size   : Pointer := 7;

      begin
         --  If the operand is not greedy, insert an extra operand before it

         if not Greedy then
            Size := Size + 3;
         end if;

         --  Move the operand in the byte-compilation, so that we can insert
         --  the operator before it.

         if Emit_Code then
            Program (Operand + Size .. Emit_Ptr + Size) :=
              Program (Operand .. Emit_Ptr);
         end if;

         --  Insert the operator at the position previously occupied by the
         --  operand.

         Emit_Ptr := Operand;

         if not Greedy then
            Old := Emit_Node (MINMOD);
            Link_Tail (Old, Old + 3);
         end if;

         Old := Emit_Node (Op);
         Emit_Natural (Old + 3, Min);
         Emit_Natural (Old + 5, Max);

         Emit_Ptr := Dest + Size;
      end Insert_Curly_Operator;

      ---------------------
      -- Insert_Operator --
      ---------------------

      procedure Insert_Operator
        (Op      : Opcode;
         Operand : Pointer;
         Greedy  : Boolean := True)
      is
         Dest   : constant Pointer := Emit_Ptr;
         Old    : Pointer;
         Size   : Pointer := 3;

      begin
         --  If not greedy, we have to emit another opcode first

         if not Greedy then
            Size := Size + 3;
         end if;

         --  Move the operand in the byte-compilation, so that we can insert
         --  the operator before it.

         if Emit_Code then
            Program (Operand + Size .. Emit_Ptr + Size)
              := Program (Operand .. Emit_Ptr);
         end if;

         --  Insert the operator at the position previously occupied by the
         --  operand.

         Emit_Ptr := Operand;

         if not Greedy then
            Old := Emit_Node (MINMOD);
            Link_Tail (Old, Old + 3);
         end if;

         Old := Emit_Node (Op);
         Emit_Ptr := Dest + Size;
      end Insert_Operator;

      -----------------------
      -- Is_Curly_Operator --
      -----------------------

      function Is_Curly_Operator (IP : Natural) return Boolean is
         Scan : Natural := IP;

      begin
         if Expression (Scan) /= '{'
           or else Scan + 2 > Expression'Last
           or else not Is_Digit (Expression (Scan + 1))
         then
            return False;
         end if;

         Scan := Scan + 1;

         --  The first digit

         loop
            Scan := Scan + 1;

            if Scan > Expression'Last then
               return False;
            end if;

            exit when not Is_Digit (Expression (Scan));
         end loop;

         if Expression (Scan) = ',' then
            loop
               Scan := Scan + 1;

               if Scan > Expression'Last then
                  return False;
               end if;

               exit when not Is_Digit (Expression (Scan));
            end loop;
         end if;

         return Expression (Scan) = '}';
      end Is_Curly_Operator;

      -------------
      -- Is_Mult --
      -------------

      function Is_Mult (IP : Natural) return Boolean is
         C : constant Character := Expression (IP);

      begin
         return     C = '*'
           or else  C = '+'
           or else  C = '?'
           or else (C = '{' and then Is_Curly_Operator (IP));
      end Is_Mult;

      -----------------------
      -- Link_Operand_Tail --
      -----------------------

      procedure Link_Operand_Tail (P, Val : Pointer) is
      begin
         if Emit_Code and then Program (P) = BRANCH then
            Link_Tail (Operand (P), Val);
         end if;
      end Link_Operand_Tail;

      ---------------
      -- Link_Tail --
      ---------------

      procedure Link_Tail (P, Val : Pointer) is
         Scan   : Pointer;
         Temp   : Pointer;
         Offset : Pointer;

      begin
         if not Emit_Code then
            return;
         end if;

         --  Find last node

         Scan := P;
         loop
            Temp := Next_Instruction (Scan);
            exit when Temp = 0;
            Scan := Temp;
         end loop;

         Offset := Val - Scan;

         Emit_Natural (Scan + 1, Natural (Offset));
      end Link_Tail;

      ----------------------
      -- Next_Instruction --
      ----------------------

      function Next_Instruction (P : Pointer) return Pointer is
         Offset : Pointer;

      begin
         if not Emit_Code then
            return 0;
         end if;

         Offset := Get_Next_Offset (Program, P);

         if Offset = 0 then
            return 0;
         end if;

         return P + Offset;
      end Next_Instruction;

      -----------
      -- Parse --
      -----------

      --  Combining parenthesis handling with the base level
      --  of regular expression is a trifle forced, but the
      --  need to tie the tails of the branches to what follows
      --  makes it hard to avoid.

      procedure Parse
        (Parenthesized  : in Boolean;
         Flags          : in out Expression_Flags;
         IP             : out Pointer)
      is
         E              : String renames Expression;
         Br             : Pointer;
         Ender          : Pointer;
         Par_No         : Natural;
         New_Flags      : Expression_Flags;
         Have_Branch    : Boolean := False;

      begin
         Flags := (Has_Width => True, others => False);  -- Tentatively

         --  Make an OPEN node, if parenthesized

         if Parenthesized then
            if Matcher.Paren_Count > Max_Paren_Count then
               Fail ("too many ()");
            end if;

            Par_No := Matcher.Paren_Count + 1;
            Matcher.Paren_Count := Matcher.Paren_Count + 1;
            IP := Emit_Node (OPEN);
            Emit (Character'Val (Par_No));

         else
            IP := 0;
            Par_No := 0;
         end if;

         --  Pick up the branches, linking them together

         Parse_Branch (New_Flags, True, Br);

         if Br = 0 then
            IP := 0;
            return;
         end if;

         if Parse_Pos <= Parse_End
           and then E (Parse_Pos) = '|'
         then
            Insert_Operator (BRANCH, Br);
            Have_Branch := True;
         end if;

         if IP /= 0 then
            Link_Tail (IP, Br);   -- OPEN -> first
         else
            IP := Br;
         end if;

         if not New_Flags.Has_Width then
            Flags.Has_Width := False;
         end if;

         Flags.SP_Start := Flags.SP_Start or New_Flags.SP_Start;

         while Parse_Pos <= Parse_End
           and then (E (Parse_Pos) = '|')
         loop
            Parse_Pos := Parse_Pos + 1;
            Parse_Branch (New_Flags, False, Br);

            if Br = 0 then
               IP := 0;
               return;
            end if;

            Link_Tail (IP, Br);   -- BRANCH -> BRANCH

            if not New_Flags.Has_Width then
               Flags.Has_Width := False;
            end if;

            Flags.SP_Start := Flags.SP_Start or New_Flags.SP_Start;
         end loop;

         --  Make a closing node, and hook it on the end

         if Parenthesized then
            Ender := Emit_Node (CLOSE);
            Emit (Character'Val (Par_No));
         else
            Ender := Emit_Node (EOP);
         end if;

         Link_Tail (IP, Ender);

         if Have_Branch then

            --  Hook the tails of the branches to the closing node

            Br := IP;
            loop
               exit when Br = 0;
               Link_Operand_Tail (Br, Ender);
               Br := Next_Instruction (Br);
            end loop;
         end if;

         --  Check for proper termination

         if Parenthesized then
            if Parse_Pos > Parse_End or else E (Parse_Pos) /= ')' then
               Fail ("unmatched ()");
            end if;

            Parse_Pos := Parse_Pos + 1;

         elsif Parse_Pos <= Parse_End then
            if E (Parse_Pos) = ')'  then
               Fail ("unmatched ()");
            else
               Fail ("junk on end");         -- "Can't happen"
            end if;
         end if;
      end Parse;

      ----------------
      -- Parse_Atom --
      ----------------

      procedure Parse_Atom
        (Expr_Flags : in out Expression_Flags;
         IP         : out Pointer)
      is
         C : Character;

      begin
         --  Tentatively set worst expression case

         Expr_Flags := Worst_Expression;

         C := Expression (Parse_Pos);
         Parse_Pos := Parse_Pos + 1;

         case (C) is
            when '^' =>
               if (Flags and Multiple_Lines) /= 0  then
                  IP := Emit_Node (MBOL);
               elsif (Flags and Single_Line) /= 0 then
                  IP := Emit_Node (SBOL);
               else
                  IP := Emit_Node (BOL);
               end if;

            when '$' =>
               if (Flags and Multiple_Lines) /= 0  then
                  IP := Emit_Node (MEOL);
               elsif (Flags and Single_Line) /= 0 then
                  IP := Emit_Node (SEOL);
               else
                  IP := Emit_Node (EOL);
               end if;

            when '.' =>
               if (Flags and Single_Line) /= 0 then
                  IP := Emit_Node (SANY);
               else
                  IP := Emit_Node (ANY);
               end if;

               Expr_Flags.Has_Width := True;
               Expr_Flags.Simple := True;

            when '[' =>
               Parse_Character_Class (IP);
               Expr_Flags.Has_Width := True;
               Expr_Flags.Simple := True;

            when '(' =>
               declare
                  New_Flags : Expression_Flags;

               begin
                  Parse (True, New_Flags, IP);

                  if IP = 0 then
                     return;
                  end if;

                  Expr_Flags.Has_Width :=
                    Expr_Flags.Has_Width or New_Flags.Has_Width;
                  Expr_Flags.SP_Start :=
                    Expr_Flags.SP_Start or New_Flags.SP_Start;
               end;

            when '|' | ASCII.LF | ')' =>
               Fail ("internal urp");  --  Supposed to be caught earlier

            when '?' | '+' | '*' | '{' =>
               Fail ("?+*{ follows nothing");

            when '\' =>
               if Parse_Pos > Parse_End then
                  Fail ("trailing \");
               end if;

               Parse_Pos := Parse_Pos + 1;

               case Expression (Parse_Pos - 1) is
                  when 'b'        =>
                     IP := Emit_Node (BOUND);

                  when 'B'        =>
                     IP := Emit_Node (NBOUND);

                  when 's'        =>
                     IP := Emit_Node (SPACE);
                     Expr_Flags.Simple := True;
                     Expr_Flags.Has_Width := True;

                  when 'S'        =>
                     IP := Emit_Node (NSPACE);
                     Expr_Flags.Simple := True;
                     Expr_Flags.Has_Width := True;

                  when 'd'        =>
                     IP := Emit_Node (DIGIT);
                     Expr_Flags.Simple := True;
                     Expr_Flags.Has_Width := True;

                  when 'D'        =>
                     IP := Emit_Node (NDIGIT);
                     Expr_Flags.Simple := True;
                     Expr_Flags.Has_Width := True;

                  when 'w'        =>
                     IP := Emit_Node (ALNUM);
                     Expr_Flags.Simple := True;
                     Expr_Flags.Has_Width := True;

                  when 'W'        =>
                     IP := Emit_Node (NALNUM);
                     Expr_Flags.Simple := True;
                     Expr_Flags.Has_Width := True;

                  when 'A'        =>
                     IP := Emit_Node (SBOL);

                  when 'G'        =>
                     IP := Emit_Node (SEOL);

                  when '0' .. '9' =>
                     IP := Emit_Node (REFF);

                     declare
                        Save : Natural := Parse_Pos - 1;

                     begin
                        while Parse_Pos <= Expression'Last
                          and then Is_Digit (Expression (Parse_Pos))
                        loop
                           Parse_Pos := Parse_Pos + 1;
                        end loop;

                        Emit (Character'Val (Natural'Value
                               (Expression (Save .. Parse_Pos - 1))));
                     end;

                  when others =>
                     Parse_Pos := Parse_Pos - 1;
                     Parse_Literal (Expr_Flags, IP);
               end case;

            when others =>
               Parse_Literal (Expr_Flags, IP);
         end case;
      end Parse_Atom;

      ------------------
      -- Parse_Branch --
      ------------------

      procedure Parse_Branch
        (Flags : in out Expression_Flags;
         First : Boolean;
         IP    : out Pointer)
      is
         E         : String renames Expression;
         Chain     : Pointer;
         Last      : Pointer;
         New_Flags : Expression_Flags;
         Dummy     : Pointer;

      begin
         Flags := Worst_Expression;    -- Tentatively

         if First then
            IP := Emit_Ptr;
         else
            IP := Emit_Node (BRANCH);
         end if;

         Chain := 0;

         while Parse_Pos <= Parse_End
           and then E (Parse_Pos) /= ')'
           and then E (Parse_Pos) /= ASCII.LF
           and then E (Parse_Pos) /= '|'
         loop
            Parse_Piece (New_Flags, Last);

            if Last = 0 then
               IP := 0;
               return;
            end if;

            Flags.Has_Width := Flags.Has_Width or New_Flags.Has_Width;

            if Chain = 0 then            -- First piece
               Flags.SP_Start := Flags.SP_Start or New_Flags.SP_Start;
            else
               Link_Tail (Chain, Last);
            end if;

            Chain := Last;
         end loop;

         if Chain = 0 then            -- Loop ran zero CURLY
            Dummy := Emit_Node (NOTHING);
         end if;

      end Parse_Branch;

      ---------------------------
      -- Parse_Character_Class --
      ---------------------------

      procedure Parse_Character_Class (IP : out Pointer) is
         Bitmap      : Character_Class;
         Invert      : Boolean := False;
         In_Range    : Boolean := False;
         Named_Class : Std_Class := ANYOF_NONE;
         Value       : Character;
         Last_Value  : Character := ASCII.Nul;

      begin
         Reset_Class (Bitmap);

         --  Do we have an invert character class ?

         if Parse_Pos <= Parse_End
           and then Expression (Parse_Pos) = '^'
         then
            Invert := True;
            Parse_Pos := Parse_Pos + 1;
         end if;

         --  First character can be ] or -, without closing the class.

         if Parse_Pos <= Parse_End
           and then (Expression (Parse_Pos) = ']'
                      or else Expression (Parse_Pos) = '-')
         then
            Set_In_Class (Bitmap, Expression (Parse_Pos));
            Parse_Pos := Parse_Pos + 1;
         end if;

         --  While we don't have the end of the class

         while Parse_Pos <= Parse_End
           and then Expression (Parse_Pos) /= ']'
         loop
            Named_Class := ANYOF_NONE;
            Value := Expression (Parse_Pos);
            Parse_Pos := Parse_Pos + 1;

            --  Do we have a Posix character class
            if Value = '[' then
               Named_Class := Parse_Posix_Character_Class;

            elsif Value = '\' then
               if Parse_Pos = Parse_End then
                  Fail ("Trailing \");
               end if;
               Value := Expression (Parse_Pos);
               Parse_Pos := Parse_Pos + 1;

               case Value is
                  when 'w' => Named_Class := ANYOF_ALNUM;
                  when 'W' => Named_Class := ANYOF_NALNUM;
                  when 's' => Named_Class := ANYOF_SPACE;
                  when 'S' => Named_Class := ANYOF_NSPACE;
                  when 'd' => Named_Class := ANYOF_DIGIT;
                  when 'D' => Named_Class := ANYOF_NDIGIT;
                  when 'n' => Value := ASCII.LF;
                  when 'r' => Value := ASCII.CR;
                  when 't' => Value := ASCII.HT;
                  when 'f' => Value := ASCII.FF;
                  when 'e' => Value := ASCII.ESC;
                  when 'a' => Value := ASCII.BEL;

                  --  when 'x'  => ??? hexadecimal value
                  --  when 'c'  => ??? control character
                  --  when '0'..'9' => ??? octal character

                  when others => null;
               end case;
            end if;

            --  Do we have a character class?

            if Named_Class /= ANYOF_NONE then

               --  A range like 'a-\d' or 'a-[:digit:] is not a range

               if In_Range then
                  Set_In_Class (Bitmap, Last_Value);
                  Set_In_Class (Bitmap, '-');
                  In_Range := False;
               end if;

               --  Expand the range

               case Named_Class is
                  when ANYOF_NONE => null;

                  when ANYOF_ALNUM | ANYOF_ALNUMC =>
                     for Value in Class_Byte'Range loop
                        if Is_Alnum (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NALNUM | ANYOF_NALNUMC =>
                     for Value in Class_Byte'Range loop
                        if not Is_Alnum (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_SPACE =>
                     for Value in Class_Byte'Range loop
                        if Is_Space (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NSPACE =>
                     for Value in Class_Byte'Range loop
                        if not Is_Space (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_DIGIT =>
                     for Value in Class_Byte'Range loop
                        if Is_Digit (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NDIGIT =>
                     for Value in Class_Byte'Range loop
                        if not Is_Digit (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_ALPHA =>
                     for Value in Class_Byte'Range loop
                        if Is_Letter (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NALPHA =>
                     for Value in Class_Byte'Range loop
                        if not Is_Letter (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_ASCII =>
                     for Value in 0 .. 127 loop
                        Set_In_Class (Bitmap, Character'Val (Value));
                     end loop;

                  when ANYOF_NASCII =>
                     for Value in 128 .. 255 loop
                        Set_In_Class (Bitmap, Character'Val (Value));
                     end loop;

                  when ANYOF_CNTRL =>
                     for Value in Class_Byte'Range loop
                        if Is_Control (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NCNTRL =>
                     for Value in Class_Byte'Range loop
                        if not Is_Control (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_GRAPH =>
                     for Value in Class_Byte'Range loop
                        if Is_Graphic (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NGRAPH =>
                     for Value in Class_Byte'Range loop
                        if not Is_Graphic (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_LOWER =>
                     for Value in Class_Byte'Range loop
                        if Is_Lower (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NLOWER =>
                     for Value in Class_Byte'Range loop
                        if not Is_Lower (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_PRINT =>
                     for Value in Class_Byte'Range loop
                        if Is_Printable (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NPRINT =>
                     for Value in Class_Byte'Range loop
                        if not Is_Printable (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_PUNCT =>
                     for Value in Class_Byte'Range loop
                        if Is_Printable (Character'Val (Value))
                          and then not Is_Space (Character'Val (Value))
                          and then not Is_Alnum (Character'Val (Value))
                        then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NPUNCT =>
                     for Value in Class_Byte'Range loop
                        if not Is_Printable (Character'Val (Value))
                          or else Is_Space (Character'Val (Value))
                          or else Is_Alnum (Character'Val (Value))
                        then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_UPPER =>
                     for Value in Class_Byte'Range loop
                        if Is_Upper (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NUPPER =>
                     for Value in Class_Byte'Range loop
                        if not Is_Upper (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_XDIGIT =>
                     for Value in Class_Byte'Range loop
                        if Is_Hexadecimal_Digit (Character'Val (Value)) then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

                  when ANYOF_NXDIGIT =>
                     for Value in Class_Byte'Range loop
                        if not Is_Hexadecimal_Digit
                          (Character'Val (Value))
                        then
                           Set_In_Class (Bitmap, Character'Val (Value));
                        end if;
                     end loop;

               end case;

            --  Not a character range

            elsif not In_Range then
               Last_Value := Value;

               if Expression (Parse_Pos) = '-'
                 and then Parse_Pos < Parse_End
                 and then Expression (Parse_Pos + 1) /= ']'
               then
                  Parse_Pos := Parse_Pos + 1;

                  --  Do we have a range like '\d-a' and '[:space:]-a'
                  --  which is not a real range

                  if Named_Class /= ANYOF_NONE then
                     Set_In_Class (Bitmap, '-');
                  else
                     In_Range := True;
                  end if;

               else
                  Set_In_Class (Bitmap, Value);

               end if;

            --  Else in a character range

            else
               if Last_Value > Value then
                  Fail ("Invalid Range [" & Last_Value'Img
                        & "-" & Value'Img & "]");
               end if;

               while Last_Value <= Value loop
                  Set_In_Class (Bitmap, Last_Value);
                  Last_Value := Character'Succ (Last_Value);
               end loop;

               In_Range := False;

            end if;

         end loop;

         --  Optimize case-insensitive ranges (put the upper case or lower
         --  case character into the bitmap)

         if (Flags and Case_Insensitive) /= 0 then
            for C in Character'Range loop
               if Get_From_Class (Bitmap, C) then
                  Set_In_Class (Bitmap, To_Lower (C));
                  Set_In_Class (Bitmap, To_Upper (C));
               end if;
            end loop;
         end if;

         --  Optimize inverted classes

         if Invert then
            for J in Bitmap'Range loop
               Bitmap (J) := not Bitmap (J);
            end loop;
         end if;

         Parse_Pos := Parse_Pos + 1;

         --  Emit the class

         IP := Emit_Node (ANYOF);
         Emit_Class (Bitmap);
      end Parse_Character_Class;

      -------------------
      -- Parse_Literal --
      -------------------

      --  This is a bit tricky due to quoted chars and due to
      --  the multiplier characters '*', '+', and '?' that
      --  take the SINGLE char previous as their operand.
      --
      --  On entry, the character at Parse_Pos - 1 is going to go
      --  into the string, no matter what it is. It could be
      --  following a \ if Parse_Atom was entered from the '\' case.
      --
      --  Basic idea is to pick up a good char in C and examine
      --  the next char. If Is_Mult (C) then twiddle, if it's a \
      --  then frozzle and if it's another magic char then push C and
      --  terminate the string. If none of the above, push C on the
      --  string and go around again.
      --
      --  Start_Pos is used to remember where "the current character"
      --  starts in the string, if due to an Is_Mult we need to back
      --  up and put the current char in a separate 1-character string.
      --  When Start_Pos is 0, C is the only char in the string;
      --  this is used in Is_Mult handling, and in setting the SIMPLE
      --  flag at the end.

      procedure Parse_Literal
        (Expr_Flags : in out Expression_Flags;
         IP         : out Pointer)
      is
         Start_Pos  : Natural := 0;
         C          : Character;
         Length_Ptr : Pointer;
         Has_Special_Operator : Boolean := False;

      begin
         Parse_Pos := Parse_Pos - 1;      --  Look at current character

         if (Flags and Case_Insensitive) /= 0 then
            IP := Emit_Node (EXACTF);
         else
            IP := Emit_Node (EXACT);
         end if;

         Length_Ptr := Emit_Ptr;
         Emit_Ptr := String_Operand (IP);

         Parse_Loop :
         loop

            C := Expression (Parse_Pos); --  Get current character

            case C is
               when '.' | '[' | '(' | ')' | '|' | ASCII.LF | '$' | '^' =>

                  if Start_Pos = 0 then
                     Start_Pos := Parse_Pos;
                     Emit (C);         --  First character is always emitted
                  else
                     exit Parse_Loop;  --  Else we are done
                  end if;

               when '?' | '+' | '*' | '{' =>

                  if Start_Pos = 0 then
                     Start_Pos := Parse_Pos;
                     Emit (C);         --  First character is always emitted

                  --  Are we looking at an operator, or is this
                  --  simply a normal character ?
                  elsif not Is_Mult (Parse_Pos) then
                     Start_Pos := Parse_Pos;
                     Case_Emit (C);
                  else
                     --  We've got something like "abc?d".  Mark this as a
                     --  special case. What we want to emit is a first
                     --  constant string for "ab", then one for "c" that will
                     --  ultimately be transformed with a CURLY operator, A
                     --  special case has to be handled for "a?", since there
                     --  is no initial string to emit.
                     Has_Special_Operator := True;
                     exit Parse_Loop;
                  end if;

               when '\' =>
                  Start_Pos := Parse_Pos;
                  if Parse_Pos = Parse_End then
                     Fail ("Trailing \");
                  else
                     case Expression (Parse_Pos + 1) is
                        when 'b' | 'B' | 's' | 'S' | 'd' | 'D'
                          | 'w' | 'W' | '0' .. '9' | 'G' | 'A'
                          => exit Parse_Loop;
                        when 'n'         => Emit (ASCII.LF);
                        when 't'         => Emit (ASCII.HT);
                        when 'r'         => Emit (ASCII.CR);
                        when 'f'         => Emit (ASCII.FF);
                        when 'e'         => Emit (ASCII.ESC);
                        when 'a'         => Emit (ASCII.BEL);
                        when others      => Emit (Expression (Parse_Pos + 1));
                     end case;
                     Parse_Pos := Parse_Pos + 1;
                  end if;

               when others =>
                  Start_Pos := Parse_Pos;
                  Case_Emit (C);
            end case;

            exit Parse_Loop when Emit_Ptr - Length_Ptr = 254;

            Parse_Pos := Parse_Pos + 1;

            exit Parse_Loop when Parse_Pos > Parse_End;
         end loop Parse_Loop;

         --  Is the string followed by a '*+?{' operator ? If yes, and if there
         --  is an initial string to emit, do it now.

         if Has_Special_Operator
           and then Emit_Ptr >= Length_Ptr + 3
         then
            Emit_Ptr := Emit_Ptr - 1;
            Parse_Pos := Start_Pos;
         end if;

         if Emit_Code then
            Program (Length_Ptr) := Character'Val (Emit_Ptr - Length_Ptr - 2);
         end if;

         Expr_Flags.Has_Width := True;

         --  Slight optimization when there is a single character

         if Emit_Ptr = Length_Ptr + 2 then
            Expr_Flags.Simple := True;
         end if;
      end Parse_Literal;

      -----------------
      -- Parse_Piece --
      -----------------

      --  Note that the branching code sequences used for '?' and the
      --  general cases of '*' and + are somewhat optimized: they use
      --  the same NOTHING node as both the endmarker for their branch
      --  list and the body of the last branch. It might seem that
      --  this node could be dispensed with entirely, but the endmarker
      --  role is not redundant.

      procedure Parse_Piece
        (Expr_Flags : in out Expression_Flags;
         IP    : out Pointer)
      is
         Op        : Character;
         New_Flags : Expression_Flags;
         Greedy    : Boolean := True;

      begin
         Parse_Atom (New_Flags, IP);

         if IP = 0 then
            return;
         end if;

         if Parse_Pos > Parse_End
           or else not Is_Mult (Parse_Pos)
         then
            Expr_Flags := New_Flags;
            return;
         end if;

         Op := Expression (Parse_Pos);

         if Op /= '+' then
            Expr_Flags := (SP_Start => True, others => False);
         else
            Expr_Flags := (Has_Width => True, others => False);
         end if;

         --  Detect non greedy operators in the easy cases

         if Op /= '{'
           and then Parse_Pos + 1 <= Parse_End
           and then Expression (Parse_Pos + 1) = '?'
         then
            Greedy := False;
            Parse_Pos := Parse_Pos + 1;
         end if;

         --  Generate the byte code

         case Op is
            when '*' =>

               if New_Flags.Simple then
                  Insert_Operator (STAR, IP, Greedy);
               else
                  Link_Tail (IP, Emit_Node (WHILEM));
                  Insert_Curly_Operator
                    (CURLYX, 0, Max_Curly_Repeat, IP, Greedy);
                  Link_Tail (IP, Emit_Node (NOTHING));
               end if;

            when '+' =>

               if New_Flags.Simple then
                  Insert_Operator (PLUS, IP, Greedy);
               else
                  Link_Tail (IP, Emit_Node (WHILEM));
                  Insert_Curly_Operator
                    (CURLYX, 1, Max_Curly_Repeat, IP, Greedy);
                  Link_Tail (IP, Emit_Node (NOTHING));
               end if;

            when '?' =>
               if New_Flags.Simple then
                  Insert_Curly_Operator (CURLY, 0, 1, IP, Greedy);
               else
                  Link_Tail (IP, Emit_Node (WHILEM));
                  Insert_Curly_Operator (CURLYX, 0, 1, IP, Greedy);
                  Link_Tail (IP, Emit_Node (NOTHING));
               end if;

            when '{' =>
               declare
                  Min, Max : Natural;

               begin
                  Get_Curly_Arguments (Parse_Pos, Min, Max, Greedy);

                  if New_Flags.Simple then
                     Insert_Curly_Operator (CURLY, Min, Max, IP, Greedy);
                  else
                     Link_Tail (IP, Emit_Node (WHILEM));
                     Insert_Curly_Operator (CURLYX, Min, Max, IP, Greedy);
                     Link_Tail (IP, Emit_Node (NOTHING));
                  end if;
               end;

            when others =>
               null;
         end case;

         Parse_Pos := Parse_Pos + 1;

         if Parse_Pos <= Parse_End
           and then Is_Mult (Parse_Pos)
         then
            Fail ("nested *+{");
         end if;
      end Parse_Piece;

      ---------------------------------
      -- Parse_Posix_Character_Class --
      ---------------------------------

      function Parse_Posix_Character_Class return Std_Class is
         Invert : Boolean := False;
         Class  : Std_Class := ANYOF_NONE;
         E      : String renames Expression;

      begin
         if Parse_Pos <= Parse_End
           and then Expression (Parse_Pos) = ':'
         then
            Parse_Pos := Parse_Pos + 1;

            --  Do we have something like:  [[:^alpha:]]

            if Parse_Pos <= Parse_End
              and then Expression (Parse_Pos) = '^'
            then
               Invert := True;
               Parse_Pos := Parse_Pos + 1;
            end if;

            --  All classes have 6 characters at least
            --  ??? magid constant 6 should have a name!

            if Parse_Pos + 6 <= Parse_End then

               case Expression (Parse_Pos) is
                  when 'a' =>
                     if E (Parse_Pos .. Parse_Pos + 4) = "alnum:]" then
                        if Invert then
                           Class := ANYOF_NALNUMC;
                        else
                           Class := ANYOF_ALNUMC;
                        end if;

                     elsif E (Parse_Pos .. Parse_Pos + 6) = "alpha:]" then
                        if Invert then
                           Class := ANYOF_NALPHA;
                        else
                           Class := ANYOF_ALPHA;
                        end if;

                     elsif E (Parse_Pos .. Parse_Pos + 6) = "ascii:]" then
                        if Invert then
                           Class := ANYOF_NASCII;
                        else
                           Class := ANYOF_ASCII;
                        end if;

                     end if;

                  when 'c' =>
                     if E (Parse_Pos .. Parse_Pos + 6) = "cntrl:]" then
                        if Invert then
                           Class := ANYOF_NCNTRL;
                        else
                           Class := ANYOF_CNTRL;
                        end if;
                     end if;

                  when 'd' =>

                     if E (Parse_Pos .. Parse_Pos + 6) = "digit:]" then
                        if Invert then
                           Class := ANYOF_NDIGIT;
                        else
                           Class := ANYOF_DIGIT;
                        end if;
                     end if;

                  when 'g' =>

                     if E (Parse_Pos .. Parse_Pos + 6) = "graph:]" then
                        if Invert then
                           Class := ANYOF_NGRAPH;
                        else
                           Class := ANYOF_GRAPH;
                        end if;
                     end if;

                  when 'l' =>

                     if E (Parse_Pos .. Parse_Pos + 6) = "lower:]" then
                        if Invert then
                           Class := ANYOF_NLOWER;
                        else
                           Class := ANYOF_LOWER;
                        end if;
                     end if;

                  when 'p' =>

                     if E (Parse_Pos .. Parse_Pos + 6) = "print:]" then
                        if Invert then
                           Class := ANYOF_NPRINT;
                        else
                           Class := ANYOF_PRINT;
                        end if;

                     elsif E (Parse_Pos .. Parse_Pos + 6) = "punct:]" then
                        if Invert then
                           Class := ANYOF_NPUNCT;
                        else
                           Class := ANYOF_PUNCT;
                        end if;
                     end if;

                  when 's' =>

                     if E (Parse_Pos .. Parse_Pos + 6) = "space:]" then
                        if Invert then
                           Class := ANYOF_NSPACE;
                        else
                           Class := ANYOF_SPACE;
                        end if;
                     end if;

                  when 'u' =>

                     if E (Parse_Pos .. Parse_Pos + 6) = "upper:]" then
                        if Invert then
                           Class := ANYOF_NUPPER;
                        else
                           Class := ANYOF_UPPER;
                        end if;
                     end if;

                  when 'w' =>

                     if E (Parse_Pos .. Parse_Pos + 5) = "word:]" then
                        if Invert then
                           Class := ANYOF_NALNUM;
                        else
                           Class := ANYOF_ALNUM;
                        end if;

                        Parse_Pos := Parse_Pos - 1;
                     end if;

                  when 'x' =>

                     if Parse_Pos + 7 <= Parse_End
                       and then E (Parse_Pos .. Parse_Pos + 7) = "xdigit:]"
                     then
                        if Invert then
                           Class := ANYOF_NXDIGIT;
                        else
                           Class := ANYOF_XDIGIT;
                        end if;

                        Parse_Pos := Parse_Pos + 1;
                     end if;

                  when others =>
                     Class := ANYOF_NONE;

               end case;

               if Class /= ANYOF_NONE then
                  Parse_Pos := Parse_Pos + 7;
               end if;

            else
               Fail ("Invalid character class");
            end if;

         else
            return ANYOF_NONE;
         end if;

         return Class;
      end Parse_Posix_Character_Class;

      Expr_Flags : Expression_Flags;
      Result     : Pointer;

   --  Start of processing for Compile

   begin
      Emit (MAGIC);
      Parse (False, Expr_Flags, Result);

      if Result = 0 then
         Fail ("Couldn't compile expression");
      end if;

      Final_Code_Size := Emit_Ptr - 1;

      --  Do we want to actually compile the expression, or simply get the
      --  code size ???

      if Emit_Code then
         Optimize (PM);
      end if;

      PM.Flags := Flags;
   end Compile;

   function Compile
     (Expression : String;
      Flags      : Regexp_Flags := No_Flags)
      return       Pattern_Matcher
   is
      Size  : Program_Size;
      Dummy : Pattern_Matcher (0);

   begin
      Compile (Dummy, Expression, Size, Flags);

      declare
         Result : Pattern_Matcher (Size);
      begin
         Compile (Result, Expression, Size, Flags);
         return Result;
      end;
   end Compile;

   procedure Compile
     (Matcher    : out Pattern_Matcher;
      Expression : String;
      Flags      : Regexp_Flags := No_Flags)
   is
      Size : Program_Size;

   begin
      Compile (Matcher, Expression, Size, Flags);
   end Compile;

   ----------
   -- Dump --
   ----------

   procedure Dump (Self : Pattern_Matcher) is

      --  Index  : Pointer := Program_First + 1;
      --  What is the above line for ???

      Op      : Opcode;
      Program : Program_Data renames Self.Program;

      procedure Dump_Until
        (Start  : Pointer;
         Till   : Pointer;
         Indent : Natural := 0);
      --  Dump the program until the node Till (not included) is met.
      --  Every line is indented with Index spaces at the beginning
      --  Dumps till the end if Till is 0.

      ----------------
      -- Dump_Until --
      ----------------

      procedure Dump_Until
        (Start  : Pointer;
         Till   : Pointer;
         Indent : Natural := 0)
      is
         Next : Pointer;
         Index : Pointer := Start;
         Local_Indent : Natural := Indent;
         Length : Pointer;

      begin
         while Index < Till loop

            Op := Opcode'Val (Character'Pos ((Self.Program (Index))));

            if Op = CLOSE then
               Local_Indent := Local_Indent - 3;
            end if;

            declare
               Point : String := Pointer'Image (Index);

            begin
               for J in 1 .. 6 - Point'Length loop
                  Put (' ');
               end loop;

               Put (Point
                    & " : "
                    & (1 .. Local_Indent => ' ')
                    & Opcode'Image (Op));
            end;

            --  Print the parenthesis number

            if Op = OPEN or else Op = CLOSE or else Op = REFF then
               Put (Natural'Image (Character'Pos (Program (Index + 3))));
            end if;

            Next := Index + Get_Next_Offset (Program, Index);

            if Next = Index then
               Put ("  (next at 0)");
            else
               Put ("  (next at " & Pointer'Image (Next) & ")");
            end if;

            case Op is

               --  Character class operand

               when ANYOF =>  null;
                  declare
                     Bitmap  : Character_Class;
                     Last    : Character := ASCII.Nul;
                     Current : Natural := 0;

                     Current_Char : Character;

                  begin
                     Bitmap_Operand (Program, Index, Bitmap);
                     Put ("   operand=");

                     while Current <= 255 loop
                        Current_Char := Character'Val (Current);

                        --  First item in a range

                        if Get_From_Class (Bitmap, Current_Char) then
                           Last := Current_Char;

                           --  Search for the last item in the range

                           loop
                              Current := Current + 1;
                              exit when Current > 255;
                              Current_Char := Character'Val (Current);
                              exit when
                                not Get_From_Class (Bitmap, Current_Char);

                           end loop;

                           if Last <= ' ' then
                              Put (Last'Img);
                           else
                              Put (Last);
                           end if;

                           if Character'Succ (Last) /= Current_Char then
                              Put ("-" & Character'Pred (Current_Char));
                           end if;

                        else
                           Current := Current + 1;
                        end if;
                     end loop;

                     New_Line;
                     Index := Index + 3 + Bitmap'Length;
                  end;

               --  string operand

               when EXACT | EXACTF =>
                  Length := String_Length (Program, Index);
                  Put ("   operand (length:" & Program_Size'Image (Length + 1)
                       & ") ="
                       & String (Program (String_Operand (Index)
                                          .. String_Operand (Index)
                                          + Length)));
                  Index := String_Operand (Index) + Length + 1;
                  New_Line;

               --  Node operand

               when BRANCH =>
                  New_Line;
                  Dump_Until (Index + 3, Next, Local_Indent + 3);
                  Index := Next;

               when STAR | PLUS =>
                  New_Line;

                  --  Only one instruction

                  Dump_Until (Index + 3, Index + 4, Local_Indent + 3);
                  Index := Next;

               when CURLY | CURLYX =>
                  Put ("  {"
                       & Natural'Image (Read_Natural (Program, Index + 3))
                       & ","
                       & Natural'Image (Read_Natural (Program, Index + 5))
                       & "}");
                  New_Line;
                  Dump_Until (Index + 7, Next, Local_Indent + 3);
                  Index := Next;

               when OPEN =>
                  New_Line;
                  Index := Index + 4;
                  Local_Indent := Local_Indent + 3;

               when CLOSE | REFF =>
                  New_Line;
                  Index := Index + 4;

               when EOP =>
                  Index := Index + 3;
                  New_Line;
                  exit;

               --  No operand

               when others =>
                  Index := Index + 3;
                  New_Line;
            end case;
         end loop;
      end Dump_Until;

   --  Start of processing for Dump

   begin
      pragma Assert (Self.Program (Program_First) = MAGIC,
                     "Corrupted Pattern_Matcher");

      Put_Line ("Must start with (Self.First) = "
                & Character'Image (Self.First));

      if (Self.Flags and Case_Insensitive) /= 0 then
         Put_Line ("  Case_Insensitive mode");
      end if;

      if (Self.Flags and Single_Line) /= 0 then
         Put_Line ("  Single_Line mode");
      end if;

      if (Self.Flags and Multiple_Lines) /= 0 then
         Put_Line ("  Multiple_Lines mode");
      end if;

      Put_Line ("     1 : MAGIC");
      Dump_Until (Program_First + 1, Self.Program'Last + 1);
   end Dump;

   --------------------
   -- Get_From_Class --
   --------------------

   function Get_From_Class
     (Bitmap : Character_Class;
      C      : Character)
      return   Boolean
   is
      Value : constant Class_Byte := Character'Pos (C);

   begin
      return (Bitmap (Value / 8)
               and Bit_Conversion (Value mod 8)) /= 0;
   end Get_From_Class;

   --------------
   -- Get_Next --
   --------------

   function Get_Next (Program : Program_Data; IP : Pointer) return Pointer is
      Offset : constant Pointer := Get_Next_Offset (Program, IP);

   begin
      if Offset = 0 then
         return 0;
      else
         return IP + Offset;
      end if;
   end Get_Next;

   ---------------------
   -- Get_Next_Offset --
   ---------------------

   function Get_Next_Offset
     (Program : Program_Data;
      IP      : Pointer)
      return    Pointer
   is
   begin
      return Pointer (Read_Natural (Program, IP + 1));
   end Get_Next_Offset;

   --------------
   -- Is_Alnum --
   --------------

   function Is_Alnum (C : Character) return Boolean is
   begin
      return Is_Alphanumeric (C) or else C = '_';
   end Is_Alnum;

   ------------------
   -- Is_Printable --
   ------------------

   function Is_Printable (C : Character) return Boolean is
      Value : constant Natural := Character'Pos (C);

   begin
      return (Value > 32 and then Value < 127)
        or else Is_Space (C);
   end Is_Printable;

   --------------
   -- Is_Space --
   --------------

   function Is_Space (C : Character) return Boolean is
   begin
      return C = ' '
        or else C = ASCII.HT
        or else C = ASCII.CR
        or else C = ASCII.LF
        or else C = ASCII.VT
        or else C = ASCII.FF;
   end Is_Space;

   -----------
   -- Match --
   -----------

   procedure Match
     (Self    : Pattern_Matcher;
      Data    : String;
      Matches : out Match_Array)
   is
      Program   : Program_Data renames Self.Program; -- Shorter notation

      --  Global work variables

      Input_Pos : Natural;          -- String-input pointer
      BOL_Pos   : Natural;          -- Beginning of input, for ^ check
      Matched   : Boolean := False;  -- Until proven True

      Matches_Full : Match_Array (0 .. Natural'Max (Self.Paren_Count,
                                                    Matches'Last));
      --  Stores the value of all the parenthesis pairs.
      --  We do not use directly Matches, so that we can also use back
      --  references (REFF) even if Matches is too small.

      type Natural_Array is array (Match_Count range <>) of Natural;
      Matches_Tmp : Natural_Array (Matches_Full'Range);
      --  Save the opening position of parenthesis.

      Last_Paren  : Natural := 0;
      --  Last parenthesis seen

      Greedy : Boolean := True;
      --  True if the next operator should be greedy

      type Current_Curly_Record;
      type Current_Curly_Access is access all Current_Curly_Record;
      type Current_Curly_Record is record
         Paren_Floor : Natural;  --  How far back to strip parenthesis data
         Cur         : Integer;  --  How many instances of scan we've matched
         Min         : Natural;  --  Minimal number of scans to match
         Max         : Natural;  --  Maximal number of scans to match
         Greedy      : Boolean;  --  Whether to work our way up or down
         Scan        : Pointer;  --  The thing to match
         Next        : Pointer;  --  What has to match after it
         Lastloc     : Natural;  --  Where we started matching this scan
         Old_Cc      : Current_Curly_Access; --  Before we started this one
      end record;
      --  Data used to handle the curly operator and the plus and star
      --  operators for complex expressions.

      Current_Curly : Current_Curly_Access := null;
      --  The curly currently being processed.

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Index (Start : Positive; C : Character) return Natural;
      --  Find character C in Data starting at Start and return position

      function Repeat
        (IP   : Pointer;
         Max  : Natural := Natural'Last)
         return Natural;
      --  Repeatedly match something simple, report how many
      --  It only matches on things of length 1.
      --  Starting from Input_Pos, it matches at most Max CURLY.

      function Try (Pos : in Positive) return Boolean;
      --  Try to match at specific point

      function Match (IP : Pointer) return Boolean;
      --  This is the main matching routine. Conceptually the strategy
      --  is simple:  check to see whether the current node matches,
      --  call self recursively to see whether the rest matches,
      --  and then act accordingly.
      --
      --  In practice Match makes some effort to avoid recursion, in
      --  particular by going through "ordinary" nodes (that don't
      --  need to know whether the rest of the match failed) by
      --  using a loop instead of recursion.

      function Match_Whilem (IP     : Pointer) return Boolean;
      --  Return True if a WHILEM matches

      function Recurse_Match (IP : Pointer; From : Natural) return Boolean;
      pragma Inline (Recurse_Match);
      --  Calls Match recursively. It saves and restores the parenthesis
      --  status and location in the input stream correctly, so that
      --  backtracking is possible

      function Match_Simple_Operator
        (Op     : Opcode;
         Scan   : Pointer;
         Next   : Pointer;
         Greedy : Boolean)
         return   Boolean;
      --  Return True it the simple operator (possibly non-greedy) matches

      pragma Inline (Index);
      pragma Inline (Repeat);

      --  These are two complex functions, but used only once.

      pragma Inline (Match_Whilem);
      pragma Inline (Match_Simple_Operator);

      -----------
      -- Index --
      -----------

      function Index
        (Start : Positive;
         C     : Character)
         return  Natural
      is
      begin
         for J in Start .. Data'Last loop
            if Data (J) = C then
               return J;
            end if;
         end loop;

         return 0;
      end Index;

      -------------------
      -- Recurse_Match --
      -------------------

      function Recurse_Match (IP : Pointer; From : Natural) return Boolean is
         L : constant Natural := Last_Paren;
         Tmp_F : constant Match_Array :=
           Matches_Full (From + 1 .. Matches_Full'Last);
         Start : constant Natural_Array :=
           Matches_Tmp (From + 1 .. Matches_Tmp'Last);
         Input : constant Natural := Input_Pos;
      begin
         if Match (IP) then
            return True;
         end if;
         Last_Paren := L;
         Matches_Full (Tmp_F'Range) := Tmp_F;
         Matches_Tmp (Start'Range) := Start;
         Input_Pos := Input;
         return False;
      end Recurse_Match;

      -----------
      -- Match --
      -----------

      function Match (IP   : Pointer) return Boolean is
         Scan   : Pointer := IP;
         Next   : Pointer;
         Op     : Opcode;

      begin
         State_Machine :
         loop
            pragma Assert (Scan /= 0);

            --  Determine current opcode and count its usage in debug mode

            Op := Opcode'Val (Character'Pos (Program (Scan)));

            --  Calculate offset of next instruction.
            --  Second character is most significant in Program_Data.

            Next := Get_Next (Program, Scan);

            case Op is
               when EOP =>
                  return True;  --  Success !

               when BRANCH =>
                  if Program (Next) /= BRANCH then
                     Next := Operand (Scan); -- No choice, avoid recursion

                  else
                     loop
                        if Recurse_Match (Operand (Scan), 0) then
                           return True;
                        end if;

                        Scan := Get_Next (Program, Scan);
                        exit when Scan = 0 or Program (Scan) /= BRANCH;
                     end loop;

                     exit State_Machine;
                  end if;

               when NOTHING =>
                  null;

               when BOL =>
                  exit State_Machine when
                    Input_Pos /= BOL_Pos
                    and then ((Self.Flags and Multiple_Lines) = 0
                              or else Data (Input_Pos - 1) /= ASCII.LF);

               when MBOL =>
                  exit State_Machine when
                    Input_Pos /= BOL_Pos
                    and then Data (Input_Pos - 1) /= ASCII.LF;

               when SBOL =>
                  exit State_Machine when Input_Pos /= BOL_Pos;

               when EOL =>
                  exit State_Machine when
                    Input_Pos <= Data'Last
                    and then ((Self.Flags and Multiple_Lines) = 0
                              or else Data (Input_Pos) /= ASCII.LF);

               when MEOL =>
                  exit State_Machine when
                    Input_Pos <= Data'Last
                    and then Data (Input_Pos) /= ASCII.LF;

               when SEOL =>
                  exit State_Machine when Input_Pos <= Data'Last;

               when BOUND | NBOUND =>

                  --  Was last char in word ?

                  declare
                     N  : Boolean := False;
                     Ln : Boolean := False;

                  begin
                     if Input_Pos /= Data'First then
                        N := Is_Alnum (Data (Input_Pos - 1));
                     end if;

                     if Input_Pos > Data'Last then
                        Ln := False;
                     else
                        Ln := Is_Alnum (Data (Input_Pos));
                     end if;

                     if Op = BOUND then
                        if N = Ln then
                           exit State_Machine;
                        end if;
                     else
                        if N /= Ln then
                           exit State_Machine;
                        end if;
                     end if;
                  end;

               when SPACE =>
                  exit State_Machine when
                    Input_Pos > Data'Last
                    or else not Is_Space (Data (Input_Pos));
                  Input_Pos := Input_Pos + 1;

               when NSPACE =>
                  exit State_Machine when
                    Input_Pos > Data'Last
                    or else Is_Space (Data (Input_Pos));
                  Input_Pos := Input_Pos + 1;

               when DIGIT =>
                  exit State_Machine when
                    Input_Pos > Data'Last
                    or else not Is_Digit (Data (Input_Pos));
                  Input_Pos := Input_Pos + 1;

               when NDIGIT =>
                  exit State_Machine when
                    Input_Pos > Data'Last
                    or else Is_Digit (Data (Input_Pos));
                  Input_Pos := Input_Pos + 1;

               when ALNUM =>
                  exit State_Machine when
                    Input_Pos > Data'Last
                    or else not Is_Alnum (Data (Input_Pos));
                  Input_Pos := Input_Pos + 1;

               when NALNUM =>
                  exit State_Machine when
                    Input_Pos > Data'Last
                    or else Is_Alnum (Data (Input_Pos));
                  Input_Pos := Input_Pos + 1;

               when ANY =>
                  exit State_Machine when Input_Pos > Data'Last
                    or else Data (Input_Pos) = ASCII.LF;
                  Input_Pos := Input_Pos + 1;

               when SANY =>
                  exit State_Machine when Input_Pos > Data'Last;
                  Input_Pos := Input_Pos + 1;

               when EXACT =>
                  declare
                     Opnd    : Pointer          := String_Operand (Scan);
                     Current : Positive         := Input_Pos;
                     Last    : constant Pointer :=
                                 Opnd + String_Length (Program, Scan);

                  begin
                     while Opnd <= Last loop
                        exit State_Machine when Current > Data'Last
                          or else Program (Opnd) /= Data (Current);
                        Current := Current + 1;
                        Opnd := Opnd + 1;
                     end loop;

                     Input_Pos := Current;
                  end;

               when EXACTF =>
                  declare
                     Opnd    : Pointer          := String_Operand (Scan);
                     Current : Positive         := Input_Pos;
                     Last    : constant Pointer :=
                                 Opnd + String_Length (Program, Scan);

                  begin
                     while Opnd <= Last loop
                        exit State_Machine when Current > Data'Last
                          or else Program (Opnd) /= To_Lower (Data (Current));
                        Current := Current + 1;
                        Opnd := Opnd + 1;
                     end loop;

                     Input_Pos := Current;
                  end;

               when ANYOF =>
                  declare
                     Bitmap : Character_Class;

                  begin
                     Bitmap_Operand (Program, Scan, Bitmap);
                     exit State_Machine when
                       Input_Pos > Data'Last
                       or else not Get_From_Class (Bitmap, Data (Input_Pos));
                     Input_Pos := Input_Pos + 1;
                  end;

               when OPEN =>
                  declare
                     No : constant Natural :=
                       Character'Pos (Program (Operand (Scan)));
                  begin
                     Matches_Tmp (No) := Input_Pos;
                  end;

               when CLOSE =>
                  declare
                     No : constant Natural :=
                       Character'Pos (Program (Operand (Scan)));
                  begin
                     Matches_Full (No) := (Matches_Tmp (No), Input_Pos - 1);
                     if Last_Paren < No then
                        Last_Paren := No;
                     end if;
                  end;

               when REFF =>
                  declare
                     No : constant Natural :=
                            Character'Pos (Program (Operand (Scan)));
                     Data_Pos : Natural;

                  begin
                     --  If we haven't seen that parenthesis yet

                     if Last_Paren < No then
                        return False;
                     end if;

                     Data_Pos := Matches_Full (No).First;
                     while Data_Pos <= Matches_Full (No).Last loop
                        if Input_Pos > Data'Last
                          or else Data (Input_Pos) /= Data (Data_Pos)
                        then
                           return False;
                        end if;

                        Input_Pos := Input_Pos + 1;
                        Data_Pos := Data_Pos + 1;
                     end loop;
                  end;

               when MINMOD =>
                  Greedy := False;

               when STAR | PLUS | CURLY =>
                  declare
                     Greed : constant Boolean := Greedy;
                  begin
                     Greedy := True;
                     return Match_Simple_Operator (Op, Scan, Next, Greed);
                  end;

               when CURLYX =>

                  --  Looking at something like:
                  --    1: CURLYX {n,m}  (->4)
                  --    2:   code for complex thing  (->3)
                  --    3:   WHILEM (->0)
                  --    4: NOTHING

                  declare
                     Cc  : aliased Current_Curly_Record;
                     Min : Natural := Read_Natural (Program, Scan + 3);
                     Max : Natural := Read_Natural (Program, Scan + 5);

                     Has_Match : Boolean;

                  begin
                     Cc := (Paren_Floor => Last_Paren,
                            Cur         => -1,
                            Min         => Min,
                            Max         => Max,
                            Greedy      => Greedy,
                            Scan        => Scan + 7,
                            Next        => Next,
                            Lastloc     => 0,
                            Old_Cc      => Current_Curly);
                     Current_Curly := Cc'Unchecked_Access;

                     Has_Match := Match (Next - 3);

                     --  Start on the WHILEM

                     Current_Curly := Cc.Old_Cc;
                     return Has_Match;
                  end;

               when WHILEM =>
                  return Match_Whilem (IP);

               when others =>
                  raise Expression_Error; -- Invalid instruction
            end case;

            Scan := Next;
         end loop State_Machine;

         --  If we get here, there is no match.
         --  For successful matches when EOP is the terminating point.

         return False;
      end Match;

      ---------------------------
      -- Match_Simple_Operator --
      ---------------------------

      function Match_Simple_Operator
        (Op     : Opcode;
         Scan   : Pointer;
         Next   : Pointer;
         Greedy : Boolean)
         return   Boolean
      is
         Next_Char       : Character := ASCII.Nul;
         Next_Char_Known : Boolean := False;
         No              : Integer;  --  Can be negative
         Min             : Natural;
         Max             : Natural := Natural'Last;
         Operand_Code    : Pointer;
         Old             : Natural;
         Last_Pos        : Natural;
         Save            : Natural := Input_Pos;

      begin
         --  Lookahead to avoid useless match attempts
         --  when we know what character comes next.

         if Program (Next) = EXACT then
            Next_Char := Program (String_Operand (Next));
            Next_Char_Known := True;
         end if;

         --  Find the minimal and maximal values for the operator

         case Op is
            when STAR =>
               Min := 0;
               Operand_Code := Operand (Scan);

            when PLUS =>
               Min := 1;
               Operand_Code := Operand (Scan);

            when others =>
               Min := Read_Natural (Program, Scan + 3);
               Max := Read_Natural (Program, Scan + 5);
               Operand_Code := Scan + 7;
         end case;

         --  Non greedy operators

         if not Greedy then
            --  Test the minimal repetitions

            if Min /= 0
              and then Repeat (Operand_Code, Min) < Min
            then
               return False;
            end if;

            Old := Input_Pos;

            --  Find the place where 'next' could work

            if Next_Char_Known then
               --  Last position to check

               Last_Pos := Input_Pos + Max;

               if Last_Pos > Data'Last
                 or else Max = Natural'Last
               then
                  Last_Pos := Data'Last;
               end if;

               --  Look for the first possible opportunity

               loop
                  --  Find the next possible position

                  while Input_Pos <= Last_Pos
                    and then Data (Input_Pos) /= Next_Char
                  loop
                     Input_Pos := Input_Pos + 1;
                  end loop;

                  if Input_Pos > Last_Pos then
                     return False;
                  end if;

                  --  Check that we still match if we stop
                  --  at the position we just found.

                  declare
                     Num : constant Natural := Input_Pos - Old;

                  begin
                     Input_Pos := Old;

                     if Repeat (Operand_Code, Num) < Num then
                        return False;
                     end if;
                  end;

                  --  Input_Pos now points to the new position

                  if Match (Get_Next (Program, Scan)) then
                     return True;
                  end if;

                  Old := Input_Pos;
                  Input_Pos := Input_Pos + 1;
               end loop;

            --  We know what the next character is

            else
               while Max >= Min loop

                  --  If the next character matches

                  if Match (Next) then
                     return True;
                  end if;

                  Input_Pos := Save + Min;

                  --  Could not or did not match -- move forward

                  if Repeat (Operand_Code, 1) /= 0 then
                     Min := Min + 1;
                  else
                     return False;
                  end if;
               end loop;
            end if;

            return False;

         --  Greedy operators

         else
            No := Repeat (Operand_Code, Max);

            --  ??? Perl has some special code here in case the
            --  next instruction is of type EOL, since $ and \Z
            --  can match before *and* after newline at the end.

            --  ??? Perl has some special code here in case (paren)
            --  is True.

            --  Else, if we don't have any parenthesis

            while No >= Min loop
               if not Next_Char_Known
                 or else (Input_Pos <= Data'Last
                           and then Data (Input_Pos) = Next_Char)
               then
                  if Match (Next) then
                     return True;
                  end if;
               end if;

               --  Could not or did not work, we back up

               No := No - 1;
               Input_Pos := Save + No;
            end loop;
            return False;
         end if;
      end Match_Simple_Operator;

      ------------------
      -- Match_Whilem --
      ------------------

      --  This is really hard to understand, because after we match what we're
      --  trying to match, we must make sure the rest of the REx is going to
      --  match for sure, and to do that we have to go back UP the parse tree
      --  by recursing ever deeper.  And if it fails, we have to reset our
      --  parent's current state that we can try again after backing off.

      function Match_Whilem (IP : Pointer) return Boolean is
         pragma Warnings (Off, IP);

         Cc : Current_Curly_Access := Current_Curly;
         N  : Natural              := Cc.Cur + 1;
         Ln : Natural              := 0;

         Lastloc : Natural := Cc.Lastloc;
         --  Detection of 0-len.

      begin
         --  If degenerate scan matches "", assume scan done.

         if Input_Pos = Cc.Lastloc
           and then N >= Cc.Min
         then
            --  Temporarily restore the old context, and check that we
            --  match was comes after CURLYX.

            Current_Curly := Cc.Old_Cc;

            if Current_Curly /= null then
               Ln := Current_Curly.Cur;
            end if;

            if Match (Cc.Next) then
               return True;
            end if;

            if Current_Curly /= null then
               Current_Curly.Cur := Ln;
            end if;

            Current_Curly := Cc;
            return False;
         end if;

         --  First, just match a string of min scans.

         if N < Cc.Min then
            Cc.Cur := N;
            Cc.Lastloc := Input_Pos;

            if Match (Cc.Scan) then
               return True;
            end if;

            Cc.Cur := N - 1;
            Cc.Lastloc := Lastloc;
            return False;
         end if;

         --  Prefer next over scan for minimal matching.

         if not Cc.Greedy then
            Current_Curly := Cc.Old_Cc;

            if Current_Curly /= null then
               Ln := Current_Curly.Cur;
            end if;

            if Recurse_Match (Cc.Next, Cc.Paren_Floor) then
               return True;
            end if;

            if Current_Curly /= null then
               Current_Curly.Cur := Ln;
            end if;

            Current_Curly := Cc;

            --  Maximum greed exceeded ?

            if N >= Cc.Max then
               return False;
            end if;

            --  Try scanning more and see if it helps
            Cc.Cur := N;
            Cc.Lastloc := Input_Pos;

            if Recurse_Match (Cc.Scan, Cc.Paren_Floor) then
               return True;
            end if;

            Cc.Cur := N - 1;
            Cc.Lastloc := Lastloc;
            return False;
         end if;

         --  Prefer scan over next for maximal matching

         if N < Cc.Max then   --  more greed allowed ?
            Cc.Cur := N;
            Cc.Lastloc := Input_Pos;

            if Recurse_Match (Cc.Scan, Cc.Paren_Floor) then
               return True;
            end if;
         end if;

         --  Failed deeper matches of scan, so see if this one works

         Current_Curly := Cc.Old_Cc;

         if Current_Curly /= null then
            Ln := Current_Curly.Cur;
         end if;

         if Match (Cc.Next) then
            return True;
         end if;

         if Current_Curly /= null then
            Current_Curly.Cur := Ln;
         end if;

         Current_Curly := Cc;
         Cc.Cur := N - 1;
         Cc.Lastloc := Lastloc;
         return False;
      end Match_Whilem;

      ------------
      -- Repeat --
      ------------

      function Repeat
        (IP   : Pointer;
         Max  : Natural := Natural'Last)
         return Natural
      is
         Scan  : Natural := Input_Pos;
         Last  : Natural;
         Op    : constant Opcode := Opcode'Val (Character'Pos (Program (IP)));
         Count : Natural;
         C     : Character;
         Is_First : Boolean := True;
         Bitmap   : Character_Class;

      begin
         if Max = Natural'Last or else Scan + Max - 1 > Data'Last then
            Last := Data'Last;
         else
            Last := Scan + Max - 1;
         end if;

         case Op is
            when ANY =>
               while Scan <= Last
                 and then Data (Scan) /= ASCII.LF
               loop
                  Scan := Scan + 1;
               end loop;

            when SANY =>
               Scan := Last + 1;

            when EXACT =>

               --  The string has only one character if Repeat was called

               C := Program (String_Operand (IP));
               while Scan <= Last
                 and then C = Data (Scan)
               loop
                  Scan := Scan + 1;
               end loop;

            when EXACTF =>

               --  The string has only one character if Repeat was called

               C := Program (String_Operand (IP));
               while Scan <= Last
                 and then To_Lower (C) = Data (Scan)
               loop
                  Scan := Scan + 1;
               end loop;

            when ANYOF =>
               if Is_First then
                  Bitmap_Operand (Program, IP, Bitmap);
                  Is_First := False;
               end if;

               while Scan <= Last
                 and then Get_From_Class (Bitmap, Data (Scan))
               loop
                  Scan := Scan + 1;
               end loop;

            when ALNUM =>
               while Scan <= Last
                 and then Is_Alnum (Data (Scan))
               loop
                  Scan := Scan + 1;
               end loop;

            when NALNUM =>
               while Scan <= Last
                 and then not Is_Alnum (Data (Scan))
               loop
                  Scan := Scan + 1;
               end loop;

            when SPACE =>
               while Scan <= Last
                 and then Is_Space (Data (Scan))
               loop
                  Scan := Scan + 1;
               end loop;

            when NSPACE =>
               while Scan <= Last
                 and then not Is_Space (Data (Scan))
               loop
                  Scan := Scan + 1;
               end loop;

            when DIGIT  =>
               while Scan <= Last
                 and then Is_Digit (Data (Scan))
               loop
                  Scan := Scan + 1;
               end loop;

            when NDIGIT  =>
               while Scan <= Last
                 and then not Is_Digit (Data (Scan))
               loop
                  Scan := Scan + 1;
               end loop;

            when others =>
               raise Program_Error;
         end case;

         Count := Scan - Input_Pos;
         Input_Pos := Scan;
         return Count;
      end Repeat;

      ---------
      -- Try --
      ---------

      function Try (Pos : in Positive) return Boolean is
      begin
         Input_Pos  := Pos;
         Last_Paren := 0;
         Matches_Full := (others => No_Match);

         if Match (Program_First + 1) then
            Matches_Full (0) := (Pos, Input_Pos - 1);
            return True;
         end if;

         return False;
      end Try;

   --  Start of processing for Match

   begin
      --  Do we have the regexp Never_Match?

      if Self.Size = 0 then
         Matches (0) := No_Match;
         return;
      end if;

      --  Check validity of program

      pragma Assert
        (Program (Program_First) = MAGIC,
         "Corrupted Pattern_Matcher");

      --  If there is a "must appear" string, look for it

      if Self.Must_Have_Length > 0 then
         declare
            First      : constant Character := Program (Self.Must_Have);
            Must_First : constant Pointer := Self.Must_Have;
            Must_Last  : constant Pointer :=
                           Must_First + Pointer (Self.Must_Have_Length - 1);
            Next_Try   : Natural := Index (Data'First, First);

         begin
            while Next_Try /= 0
              and then Data (Next_Try .. Next_Try + Self.Must_Have_Length - 1)
                          = String (Program (Must_First .. Must_Last))
            loop
               Next_Try := Index (Next_Try + 1, First);
            end loop;

            if Next_Try = 0 then
               Matches_Full := (others => No_Match);
               return;                  -- Not present
            end if;
         end;
      end if;

      --  Mark beginning of line for ^

      BOL_Pos := Data'First;

      --  Simplest case first: an anchored match need be tried only once

      if Self.Anchored and then (Self.Flags and Multiple_Lines) = 0 then
         Matched := Try (Data'First);

      elsif Self.Anchored then
         declare
            Next_Try : Natural := Data'First;
         begin
            --  Test the first position in the buffer
            Matched := Try (Next_Try);

            --  Else only test after newlines

            if not Matched then
               while Next_Try <= Data'Last loop
                  while Next_Try <= Data'Last
                    and then Data (Next_Try) /= ASCII.LF
                  loop
                     Next_Try := Next_Try + 1;
                  end loop;

                  Next_Try := Next_Try + 1;

                  if Next_Try <= Data'Last then
                     Matched := Try (Next_Try);
                     exit when Matched;
                  end if;
               end loop;
            end if;
         end;

      elsif Self.First /= ASCII.NUL then

         --  We know what char it must start with

         declare
            Next_Try : Natural := Index (Data'First, Self.First);

         begin
            while Next_Try /= 0 loop
               Matched := Try (Next_Try);
               exit when Matched;
               Next_Try := Index (Next_Try + 1, Self.First);
            end loop;
         end;

      else
         --  Messy cases: try all locations (including for the empty string)

         Matched := Try (Data'First);

         if not Matched then
            for S in Data'First + 1 .. Data'Last loop
               Matched := Try (S);
               exit when Matched;
            end loop;
         end if;
      end if;

      --  Matched has its value

      for J in Last_Paren + 1 .. Matches'Last loop
         Matches_Full (J) := No_Match;
      end loop;

      Matches := Matches_Full (Matches'Range);
      return;
   end Match;

   function  Match
     (Self : Pattern_Matcher;
      Data : String)
      return Natural
   is
      Matches : Match_Array (0 .. 0);

   begin
      Match (Self, Data, Matches);
      if Matches (0) = No_Match then
         return Data'First - 1;
      else
         return Matches (0).First;
      end if;
   end Match;

   procedure Match
     (Expression : String;
      Data       : String;
      Matches    : out Match_Array;
      Size       : Program_Size := 0)
   is
      PM            : Pattern_Matcher (Size);
      Finalize_Size : Program_Size;

   begin
      if Size = 0 then
         Match (Compile (Expression), Data, Matches);
      else
         Compile (PM, Expression, Finalize_Size);
         Match (PM, Data, Matches);
      end if;
   end Match;

   function  Match
     (Expression : String;
      Data       : String;
      Size       : Program_Size := 0)
      return       Natural
   is
      PM         : Pattern_Matcher (Size);
      Final_Size : Program_Size; -- unused

   begin
      if Size = 0 then
         return Match (Compile (Expression), Data);
      else
         Compile (PM, Expression, Final_Size);
         return Match (PM, Data);
      end if;
   end Match;

   function  Match
     (Expression : String;
      Data       : String;
      Size       : Program_Size := 0)
      return       Boolean
   is
      Matches    : Match_Array (0 .. 0);
      PM         : Pattern_Matcher (Size);
      Final_Size : Program_Size; -- unused

   begin
      if Size = 0 then
         Match (Compile (Expression), Data, Matches);
      else
         Compile (PM, Expression, Final_Size);
         Match (PM, Data, Matches);
      end if;

      return Matches (0).First >= Data'First;
   end Match;

   -------------
   -- Operand --
   -------------

   function Operand (P : Pointer) return Pointer is
   begin
      return P + 3;
   end Operand;

   --------------
   -- Optimize --
   --------------

   procedure Optimize (Self : in out Pattern_Matcher) is
      Max_Length  : Program_Size;
      This_Length : Program_Size;
      Longest     : Pointer;
      Scan        : Pointer;
      Program     : Program_Data renames Self.Program;

   begin
      --  Start with safe defaults (no optimization):
      --    *  No known first character of match
      --    *  Does not necessarily start at beginning of line
      --    *  No string known that has to appear in data

      Self.First := ASCII.NUL;
      Self.Anchored := False;
      Self.Must_Have := Program'Last + 1;
      Self.Must_Have_Length := 0;

      Scan := Program_First + 1;  --  First instruction (can be anything)

      if Program (Scan) = EXACT then
         Self.First := Program (String_Operand (Scan));

      elsif Program (Scan) = BOL
        or else Program (Scan) = SBOL
        or else Program (Scan) = MBOL
      then
         Self.Anchored := True;
      end if;

      --  If there's something expensive in the regexp, find the
      --  longest literal string that must appear and make it the
      --  regmust. Resolve ties in favor of later strings, since
      --  the regstart check works with the beginning of the regexp.
      --  and avoiding duplication strengthens checking. Not a
      --  strong reason, but sufficient in the absence of others.

      if False then -- if Flags.SP_Start then ???
         Longest := 0;
         Max_Length := 0;
         while Scan /= 0 loop
            if Program (Scan) = EXACT or else Program (Scan) = EXACTF then
               This_Length := String_Length (Program, Scan);

               if This_Length >= Max_Length then
                  Longest := String_Operand (Scan);
                  Max_Length := This_Length;
               end if;
            end if;

            Scan := Get_Next (Program, Scan);
         end loop;

         Self.Must_Have        := Longest;
         Self.Must_Have_Length := Natural (Max_Length) + 1;
      end if;
   end Optimize;

   -----------------
   -- Paren_Count --
   -----------------

   function Paren_Count (Regexp : Pattern_Matcher) return Match_Count is
   begin
      return Regexp.Paren_Count;
   end Paren_Count;

   -----------
   -- Quote --
   -----------

   function Quote (Str : String) return String is
      S    : String (1 .. Str'Length * 2);
      Last : Natural := 0;

   begin
      for J in Str'Range loop
         case Str (J) is
            when '^' | '$' | '|' | '*' | '+' | '?' | '{'
              | '}' | '[' | ']' | '(' | ')' | '\' =>

               S (Last + 1) := '\';
               S (Last + 2) := Str (J);
               Last := Last + 2;

            when others =>
               S (Last + 1) := Str (J);
               Last := Last + 1;
         end case;
      end loop;

      return S (1 .. Last);
   end Quote;

   ------------------
   -- Read_Natural --
   ------------------

   function Read_Natural
     (Program : Program_Data;
      IP      : Pointer)
      return    Natural
   is
   begin
      return Character'Pos (Program (IP)) +
               256 * Character'Pos (Program (IP + 1));
   end Read_Natural;

   -----------------
   -- Reset_Class --
   -----------------

   procedure Reset_Class (Bitmap : in out Character_Class) is
   begin
      Bitmap := (others => 0);
   end Reset_Class;

   ------------------
   -- Set_In_Class --
   ------------------

   procedure Set_In_Class
     (Bitmap : in out Character_Class;
      C      : Character)
   is
      Value : constant Class_Byte := Character'Pos (C);

   begin
      Bitmap (Value / 8) := Bitmap (Value / 8)
        or Bit_Conversion (Value mod 8);
   end Set_In_Class;

   -------------------
   -- String_Length --
   -------------------

   function String_Length
     (Program : Program_Data;
      P       : Pointer)
      return    Program_Size
   is
   begin
      pragma Assert (Program (P) = EXACT or else Program (P) = EXACTF);
      return Character'Pos (Program (P + 3));
   end String_Length;

   --------------------
   -- String_Operand --
   --------------------

   function String_Operand (P : Pointer) return Pointer is
   begin
      return P + 4;
   end String_Operand;

end GNAT.Regpat;
