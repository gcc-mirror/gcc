------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                G N A T . S P I T B O L . P A T T E R N S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1998-2001, Ada Core Technologies, Inc.           --
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

--  Note: the data structures and general approach used in this implementation
--  are derived from the original MINIMAL sources for SPITBOL. The code is not
--  a direct translation, but the approach is followed closely. In particular,
--  we use the one stack approach developed in the SPITBOL implementation.

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;

with GNAT.Debug_Utilities;      use GNAT.Debug_Utilities;

with System;                    use System;

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body GNAT.Spitbol.Patterns is

   ------------------------
   -- Internal Debugging --
   ------------------------

   Internal_Debug : constant Boolean := False;
   --  Set this flag to True to activate some built-in debugging traceback
   --  These are all lines output with PutD and Put_LineD.

   procedure New_LineD;
   pragma Inline (New_LineD);
   --  Output new blank line with New_Line if Internal_Debug is True

   procedure PutD (Str : String);
   pragma Inline (PutD);
   --  Output string with Put if Internal_Debug is True

   procedure Put_LineD (Str : String);
   pragma Inline (Put_LineD);
   --  Output string with Put_Line if Internal_Debug is True

   -----------------------------
   -- Local Type Declarations --
   -----------------------------

   subtype String_Ptr is Ada.Strings.Unbounded.String_Access;
   subtype File_Ptr   is Ada.Text_IO.File_Access;

   function To_PE_Ptr  is new Unchecked_Conversion (Address, PE_Ptr);
   function To_Address is new Unchecked_Conversion (PE_Ptr, Address);
   --  Used only for debugging output purposes

   subtype AFC is Ada.Finalization.Controlled;

   N : constant PE_Ptr := null;
   --  Shorthand used to initialize Copy fields to null

   type Character_Ptr is access all Character;
   type Natural_Ptr   is access all Natural;
   type Pattern_Ptr   is access all Pattern;

   --------------------------------------------------
   -- Description of Algorithm and Data Structures --
   --------------------------------------------------

   --  A pattern structure is represented as a linked graph of nodes
   --  with the following structure:

   --      +------------------------------------+
   --      I                Pcode               I
   --      +------------------------------------+
   --      I                Index               I
   --      +------------------------------------+
   --      I                Pthen               I
   --      +------------------------------------+
   --      I             parameter(s)           I
   --      +------------------------------------+

   --     Pcode is a code value indicating the type of the patterm node. This
   --     code is used both as the discriminant value for the record, and as
   --     the case index in the main match routine that branches to the proper
   --     match code for the given element.

   --     Index is a serial index number. The use of these serial index
   --     numbers is described in a separate section.

   --     Pthen is a pointer to the successor node, i.e the node to be matched
   --     if the attempt to match the node succeeds. If this is the last node
   --     of the pattern to be matched, then Pthen points to a dummy node
   --     of kind PC_EOP (end of pattern), which initiales pattern exit.

   --     The parameter or parameters are present for certain node types,
   --     and the type varies with the pattern code.

   type Pattern_Code is (
      PC_Arb_Y,
      PC_Assign,
      PC_Bal,
      PC_BreakX_X,
      PC_Cancel,
      PC_EOP,
      PC_Fail,
      PC_Fence,
      PC_Fence_X,
      PC_Fence_Y,
      PC_R_Enter,
      PC_R_Remove,
      PC_R_Restore,
      PC_Rest,
      PC_Succeed,
      PC_Unanchored,

      PC_Alt,
      PC_Arb_X,
      PC_Arbno_S,
      PC_Arbno_X,

      PC_Rpat,

      PC_Pred_Func,

      PC_Assign_Imm,
      PC_Assign_OnM,
      PC_Any_VP,
      PC_Break_VP,
      PC_BreakX_VP,
      PC_NotAny_VP,
      PC_NSpan_VP,
      PC_Span_VP,
      PC_String_VP,

      PC_Write_Imm,
      PC_Write_OnM,

      PC_Null,
      PC_String,

      PC_String_2,
      PC_String_3,
      PC_String_4,
      PC_String_5,
      PC_String_6,

      PC_Setcur,

      PC_Any_CH,
      PC_Break_CH,
      PC_BreakX_CH,
      PC_Char,
      PC_NotAny_CH,
      PC_NSpan_CH,
      PC_Span_CH,

      PC_Any_CS,
      PC_Break_CS,
      PC_BreakX_CS,
      PC_NotAny_CS,
      PC_NSpan_CS,
      PC_Span_CS,

      PC_Arbno_Y,
      PC_Len_Nat,
      PC_Pos_Nat,
      PC_RPos_Nat,
      PC_RTab_Nat,
      PC_Tab_Nat,

      PC_Pos_NF,
      PC_Len_NF,
      PC_RPos_NF,
      PC_RTab_NF,
      PC_Tab_NF,

      PC_Pos_NP,
      PC_Len_NP,
      PC_RPos_NP,
      PC_RTab_NP,
      PC_Tab_NP,

      PC_Any_VF,
      PC_Break_VF,
      PC_BreakX_VF,
      PC_NotAny_VF,
      PC_NSpan_VF,
      PC_Span_VF,
      PC_String_VF);

   type IndexT is range 0 .. +(2 **15 - 1);

   type PE (Pcode : Pattern_Code) is record

      Index : IndexT;
      --  Serial index number of pattern element within pattern.

      Pthen : PE_Ptr;
      --  Successor element, to be matched after this one

      case Pcode is

         when PC_Arb_Y      |
              PC_Assign     |
              PC_Bal        |
              PC_BreakX_X   |
              PC_Cancel     |
              PC_EOP        |
              PC_Fail       |
              PC_Fence      |
              PC_Fence_X    |
              PC_Fence_Y    |
              PC_Null       |
              PC_R_Enter    |
              PC_R_Remove   |
              PC_R_Restore  |
              PC_Rest       |
              PC_Succeed    |
              PC_Unanchored => null;

         when PC_Alt        |
              PC_Arb_X      |
              PC_Arbno_S    |
              PC_Arbno_X    => Alt  : PE_Ptr;

         when PC_Rpat       => PP   : Pattern_Ptr;

         when PC_Pred_Func  => BF   : Boolean_Func;

         when PC_Assign_Imm |
              PC_Assign_OnM |
              PC_Any_VP     |
              PC_Break_VP   |
              PC_BreakX_VP  |
              PC_NotAny_VP  |
              PC_NSpan_VP   |
              PC_Span_VP    |
              PC_String_VP  => VP   : VString_Ptr;

         when PC_Write_Imm  |
              PC_Write_OnM  => FP   : File_Ptr;

         when PC_String     => Str  : String_Ptr;

         when PC_String_2   => Str2 : String (1 .. 2);

         when PC_String_3   => Str3 : String (1 .. 3);

         when PC_String_4   => Str4 : String (1 .. 4);

         when PC_String_5   => Str5 : String (1 .. 5);

         when PC_String_6   => Str6 : String (1 .. 6);

         when PC_Setcur     => Var  : Natural_Ptr;

         when PC_Any_CH     |
              PC_Break_CH   |
              PC_BreakX_CH  |
              PC_Char       |
              PC_NotAny_CH  |
              PC_NSpan_CH   |
              PC_Span_CH    => Char : Character;

         when PC_Any_CS     |
              PC_Break_CS   |
              PC_BreakX_CS  |
              PC_NotAny_CS  |
              PC_NSpan_CS   |
              PC_Span_CS    => CS   : Character_Set;

         when PC_Arbno_Y    |
              PC_Len_Nat    |
              PC_Pos_Nat    |
              PC_RPos_Nat   |
              PC_RTab_Nat   |
              PC_Tab_Nat    => Nat  : Natural;

         when PC_Pos_NF     |
              PC_Len_NF     |
              PC_RPos_NF    |
              PC_RTab_NF    |
              PC_Tab_NF     => NF   : Natural_Func;

         when PC_Pos_NP     |
              PC_Len_NP     |
              PC_RPos_NP    |
              PC_RTab_NP    |
              PC_Tab_NP     => NP   : Natural_Ptr;

         when PC_Any_VF     |
              PC_Break_VF   |
              PC_BreakX_VF  |
              PC_NotAny_VF  |
              PC_NSpan_VF   |
              PC_Span_VF    |
              PC_String_VF  => VF   : VString_Func;

      end case;
   end record;

   subtype PC_Has_Alt is Pattern_Code range PC_Alt .. PC_Arbno_X;
   --  Range of pattern codes that has an Alt field. This is used in the
   --  recursive traversals, since these links must be followed.

   EOP_Element : aliased constant PE := (PC_EOP, 0, N);
   --  This is the end of pattern element, and is thus the representation of
   --  a null pattern. It has a zero index element since it is never placed
   --  inside a pattern. Furthermore it does not need a successor, since it
   --  marks the end of the pattern, so that no more successors are needed.

   EOP : constant PE_Ptr := EOP_Element'Unrestricted_Access;
   --  This is the end of pattern pointer, that is used in the Pthen pointer
   --  of other nodes to signal end of pattern.

   --  The following array is used to determine if a pattern used as an
   --  argument for Arbno is eligible for treatment using the simple Arbno
   --  structure (i.e. it is a pattern that is guaranteed to match at least
   --  one character on success, and not to make any entries on the stack.

   OK_For_Simple_Arbno :
     array (Pattern_Code) of Boolean := (
       PC_Any_CS     |
       PC_Any_CH     |
       PC_Any_VF     |
       PC_Any_VP     |
       PC_Char       |
       PC_Len_Nat    |
       PC_NotAny_CS  |
       PC_NotAny_CH  |
       PC_NotAny_VF  |
       PC_NotAny_VP  |
       PC_Span_CS    |
       PC_Span_CH    |
       PC_Span_VF    |
       PC_Span_VP    |
       PC_String     |
       PC_String_2   |
       PC_String_3   |
       PC_String_4   |
       PC_String_5   |
       PC_String_6   => True,

       others => False);

   -------------------------------
   -- The Pattern History Stack --
   -------------------------------

   --  The pattern history stack is used for controlling backtracking when
   --  a match fails. The idea is to stack entries that give a cursor value
   --  to be restored, and a node to be reestablished as the current node to
   --  attempt an appropriate rematch operation. The processing for a pattern
   --  element that has rematch alternatives pushes an appropriate entry or
   --  entry on to the stack, and the proceeds. If a match fails at any point,
   --  the top element of the stack is popped off, resetting the cursor and
   --  the match continues by accessing the node stored with this entry.

   type Stack_Entry is record

      Cursor : Integer;
      --  Saved cursor value that is restored when this entry is popped
      --  from the stack if a match attempt fails. Occasionally, this
      --  field is used to store a history stack pointer instead of a
      --  cursor. Such cases are noted in the documentation and the value
      --  stored is negative since stack pointer values are always negative.

      Node : PE_Ptr;
      --  This pattern element reference is reestablished as the current
      --  Node to be matched (which will attempt an appropriate rematch).

   end record;

   subtype Stack_Range is Integer range -Stack_Size .. -1;

   type Stack_Type is array (Stack_Range) of Stack_Entry;
   --  The type used for a history stack. The actual instance of the stack
   --  is declared as a local variable in the Match routine, to properly
   --  handle recursive calls to Match. All stack pointer values are negative
   --  to distinguish them from normal cursor values.

   --  Note: the pattern matching stack is used only to handle backtracking.
   --  If no backtracking occurs, its entries are never accessed, and never
   --  popped off, and in particular it is normal for a successful match
   --  to terminate with entries on the stack that are simply discarded.

   --  Note: in subsequent diagrams of the stack, we always place element
   --  zero (the deepest element) at the top of the page, then build the
   --  stack down on the page with the most recent (top of stack) element
   --  being the bottom-most entry on the page.

   --  Stack checking is handled by labeling every pattern with the maximum
   --  number of stack entries that are required, so a single check at the
   --  start of matching the pattern suffices. There are two exceptions.

   --  First, the count does not include entries for recursive pattern
   --  references. Such recursions must therefore perform a specific
   --  stack check with respect to the number of stack entries required
   --  by the recursive pattern that is accessed and the amount of stack
   --  that remains unused.

   --  Second, the count includes only one iteration of an Arbno pattern,
   --  so a specific check must be made on subsequent iterations that there
   --  is still enough stack space left. The Arbno node has a field that
   --  records the number of stack entries required by its argument for
   --  this purpose.

   ---------------------------------------------------
   -- Use of Serial Index Field in Pattern Elements --
   ---------------------------------------------------

   --  The serial index numbers for the pattern elements are assigned as
   --  a pattern is consructed from its constituent elements. Note that there
   --  is never any sharing of pattern elements between patterns (copies are
   --  always made), so the serial index numbers are unique to a particular
   --  pattern as referenced from the P field of a value of type Pattern.

   --  The index numbers meet three separate invariants, which are used for
   --  various purposes as described in this section.

   --  First, the numbers uniquely identify the pattern elements within a
   --  pattern. If Num is the number of elements in a given pattern, then
   --  the serial index numbers for the elements of this pattern will range
   --  from 1 .. Num, so that each element has a separate value.

   --  The purpose of this assignment is to provide a convenient auxiliary
   --  data structure mechanism during operations which must traverse a
   --  pattern (e.g. copy and finalization processing). Once constructed
   --  patterns are strictly read only. This is necessary to allow sharing
   --  of patterns between tasks. This means that we cannot go marking the
   --  pattern (e.g. with a visited bit). Instead we cosntuct a separate
   --  vector that contains the necessary information indexed by the Index
   --  values in the pattern elements. For this purpose the only requirement
   --  is that they be uniquely assigned.

   --  Second, the pattern element referenced directly, i.e. the leading
   --  pattern element, is always the maximum numbered element and therefore
   --  indicates the total number of elements in the pattern. More precisely,
   --  the element referenced by the P field of a pattern value, or the
   --  element returned by any of the internal pattern construction routines
   --  in the body (that return a value of type PE_Ptr) always is this
   --  maximum element,

   --  The purpose of this requirement is to allow an immediate determination
   --  of the number of pattern elements within a pattern. This is used to
   --  properly size the vectors used to contain auxiliary information for
   --  traversal as described above.

   --  Third, as compound pattern structures are constructed, the way in which
   --  constituent parts of the pattern are constructed is stylized. This is
   --  an automatic consequence of the way that these compounjd structures
   --  are constructed, and basically what we are doing is simply documenting
   --  and specifying the natural result of the pattern construction. The
   --  section describing compound pattern structures gives details of the
   --  numbering of each compound pattern structure.

   --  The purpose of specifying the stylized numbering structures for the
   --  compound patterns is to help simplify the processing in the Image
   --  function, since it eases the task of retrieving the original recursive
   --  structure of the pattern from the flat graph structure of elements.
   --  This use in the Image function is the only point at which the code
   --  makes use of the stylized structures.

   type Ref_Array is array (IndexT range <>) of PE_Ptr;
   --  This type is used to build an array whose N'th entry references the
   --  element in a pattern whose Index value is N. See Build_Ref_Array.

   procedure Build_Ref_Array (E : PE_Ptr; RA : out Ref_Array);
   --  Given a pattern element which is the leading element of a pattern
   --  structure, and a Ref_Array with bounds 1 .. E.Index, fills in the
   --  Ref_Array so that its N'th entry references the element of the
   --  referenced pattern whose Index value is N.

   -------------------------------
   -- Recursive Pattern Matches --
   -------------------------------

   --  The pattern primitive (+P) where P is a Pattern_Ptr or Pattern_Func
   --  causes a recursive pattern match. This cannot be handled by an actual
   --  recursive call to the outer level Match routine, since this would not
   --  allow for possible backtracking into the region matched by the inner
   --  pattern. Indeed this is the classical clash between recursion and
   --  backtracking, and a simple recursive stack structure does not suffice.

   --  This section describes how this recursion and the possible associated
   --  backtracking is handled. We still use a single stack, but we establish
   --  the concept of nested regions on this stack, each of which has a stack
   --  base value pointing to the deepest stack entry of the region. The base
   --  value for the outer level is zero.

   --  When a recursive match is established, two special stack entries are
   --  made. The first entry is used to save the original node that starts
   --  the recursive match. This is saved so that the successor field of
   --  this node is accessible at the end of the match, but it is never
   --  popped and executed.

   --  The second entry corresponds to a standard new region action. A
   --  PC_R_Remove node is stacked, whose cursor field is used to store
   --  the outer stack base, and the stack base is reset to point to
   --  this PC_R_Remove node. Then the recursive pattern is matched and
   --  it can make history stack entries in the normal matter, so now
   --  the stack looks like:

   --     (stack entries made by outer level)

   --     (Special entry, node is (+P) successor
   --      cursor entry is not used)

   --     (PC_R_Remove entry, "cursor" value is (negative)     <-- Stack base
   --      saved base value for the enclosing region)

   --     (stack entries made by inner level)

   --  If a subsequent failure occurs and pops the PC_R_Remove node, it
   --  removes itself and the special entry immediately underneath it,
   --  restores the stack base value for the enclosing region, and then
   --  again signals failure to look for alternatives that were stacked
   --  before the recursion was initiated.

   --  Now we need to consider what happens if the inner pattern succeeds, as
   --  signalled by accessing the special PC_EOP pattern primitive. First we
   --  recognize the nested case by looking at the Base value. If this Base
   --  value is Stack'First, then the entire match has succeeded, but if the
   --  base value is greater than Stack'First, then we have successfully
   --  matched an inner pattern, and processing continues at the outer level.

   --  There are two cases. The simple case is when the inner pattern has made
   --  no stack entries, as recognized by the fact that the current stack
   --  pointer is equal to the current base value. In this case it is fine to
   --  remove all trace of the recursion by restoring the outer base value and
   --  using the special entry to find the appropriate successor node.

   --  The more complex case arises when the inner match does make stack
   --  entries. In this case, the PC_EOP processing stacks a special entry
   --  whose cursor value saves the saved inner base value (the one that
   --  references the corresponding PC_R_Remove value), and whose node
   --  pointer references a PC_R_Restore node, so the stack looks like:

   --     (stack entries made by outer level)

   --     (Special entry, node is (+P) successor,
   --      cursor entry is not used)

   --     (PC_R_Remove entry, "cursor" value is (negative)
   --      saved base value for the enclosing region)

   --     (stack entries made by inner level)

   --     (PC_Region_Replace entry, "cursor" value is (negative)
   --      stack pointer value referencing the PC_R_Remove entry).

   --  If the entire match succeeds, then these stack entries are, as usual,
   --  ignored and abandoned. If on the other hand a subsequent failure
   --  causes the PC_Region_Replace entry to be popped, it restores the
   --  inner base value from its saved "cursor" value and then fails again.
   --  Note that it is OK that the cursor is temporarily clobbered by this
   --  pop, since the second failure will reestablish a proper cursor value.

   ---------------------------------
   -- Compound Pattern Structures --
   ---------------------------------

   --  This section discusses the compound structures used to represent
   --  constructed patterns. It shows the graph structures of pattern
   --  elements that are constructed, and in the case of patterns that
   --  provide backtracking possibilities, describes how the history
   --  stack is used to control the backtracking. Finally, it notes the
   --  way in which the Index numbers are assigned to the structure.

   --  In all diagrams, solid lines (built witth minus signs or vertical
   --  bars, represent successor pointers (Pthen fields) with > or V used
   --  to indicate the direction of the pointer. The initial node of the
   --  structure is in the upper left of the diagram. A dotted line is an
   --  alternative pointer from the element above it to the element below
   --  it. See individual sections for details on how alternatives are used.

      -------------------
      -- Concatenation --
      -------------------

      --  In the pattern structures listed in this section, a line that looks
      --  lile ----> with nothing to the right indicates an end of pattern
      --  (EOP) pointer that represents the end of the match.

      --  When a pattern concatenation (L & R) occurs, the resulting structure
      --  is obtained by finding all such EOP pointers in L, and replacing
      --  them to point to R. This is the most important flattening that
      --  occurs in constructing a pattern, and it means that the pattern
      --  matching circuitry does not have to keep track of the structure
      --  of a pattern with respect to concatenation, since the appropriate
      --  successor is always at hand.

      --  Concatenation itself generates no additional possibilities for
      --  backtracking, but the constituent patterns of the concatenated
      --  structure will make stack entries as usual. The maximum amount
      --  of stack required by the structure is thus simply the sum of the
      --  maximums required by L and R.

      --  The index numbering of a concatenation structure works by leaving
      --  the numbering of the right hand pattern, R, unchanged and adjusting
      --  the numbers in the left hand pattern, L up by the count of elements
      --  in R. This ensures that the maximum numbered element is the leading
      --  element as required (given that it was the leading element in L).

      -----------------
      -- Alternation --
      -----------------

      --  A pattern (L or R) constructs the structure:

      --    +---+     +---+
      --    | A |---->| L |---->
      --    +---+     +---+
      --      .
      --      .
      --    +---+
      --    | R |---->
      --    +---+

      --  The A element here is a PC_Alt node, and the dotted line represents
      --  the contents of the Alt field. When the PC_Alt element is matched,
      --  it stacks a pointer to the leading element of R on the history stack
      --  so that on subsequent failure, a match of R is attempted.

      --  The A node is the higest numbered element in the pattern. The
      --  original index numbers of R are unchanged, but the index numbers
      --  of the L pattern are adjusted up by the count of elements in R.

      --  Note that the difference between the index of the L leading element
      --  the index of the R leading element (after building the alt structure)
      --  indicates the number of nodes in L, and this is true even after the
      --  structure is incorporated into some larger structure. For example,
      --  if the A node has index 16, and L has index 15 and R has index
      --  5, then we know that L has 10 (15-5) elements in it.

      --  Suppose that we now concatenate this structure to another pattern
      --  with 9 elements in it. We will now have the A node with an index
      --  of 25, L with an index of 24 and R with an index of 14. We still
      --  know that L has 10 (24-14) elements in it, numbered 15-24, and
      --  consequently the successor of the alternation structure has an
      --  index with a value less than 15. This is used in Image to figure
      --  out the original recursive structure of a pattern.

      --  To clarify the interaction of the alternation and concatenation
      --  structures, here is a more complex example of the structure built
      --  for the pattern:

      --      (V or W or X) (Y or Z)

      --  where A,B,C,D,E are all single element patterns:

      --    +---+     +---+       +---+     +---+
      --    I A I---->I V I---+-->I A I---->I Y I---->
      --    +---+     +---+   I   +---+     +---+
      --      .               I     .
      --      .               I     .
      --    +---+     +---+   I   +---+
      --    I A I---->I W I-->I   I Z I---->
      --    +---+     +---+   I   +---+
      --      .               I
      --      .               I
      --    +---+             I
      --    I X I------------>+
      --    +---+

      --  The numbering of the nodes would be as follows:

      --    +---+     +---+       +---+     +---+
      --    I 8 I---->I 7 I---+-->I 3 I---->I 2 I---->
      --    +---+     +---+   I   +---+     +---+
      --      .               I     .
      --      .               I     .
      --    +---+     +---+   I   +---+
      --    I 6 I---->I 5 I-->I   I 1 I---->
      --    +---+     +---+   I   +---+
      --      .               I
      --      .               I
      --    +---+             I
      --    I 4 I------------>+
      --    +---+

      --  Note: The above structure actually corresponds to

      --    (A or (B or C)) (D or E)

      --  rather than

      --    ((A or B) or C) (D or E)

      --  which is the more natural interpretation, but in fact alternation
      --  is associative, and the construction of an alternative changes the
      --  left grouped pattern to the right grouped pattern in any case, so
      --  that the Image function produces a more natural looking output.

      ---------
      -- Arb --
      ---------

      --  An Arb pattern builds the structure

      --    +---+
      --    | X |---->
      --    +---+
      --      .
      --      .
      --    +---+
      --    | Y |---->
      --    +---+

      --  The X node is a PC_Arb_X node, which matches null, and stacks a
      --  pointer to Y node, which is the PC_Arb_Y node that matches one
      --  extra character and restacks itself.

      --  The PC_Arb_X node is numbered 2, and the PC_Arb_Y node is 1.

      -------------------------
      -- Arbno (simple case) --
      -------------------------

      --  The simple form of Arbno can be used where the pattern always
      --  matches at least one character if it succeeds, and it is known
      --  not to make any history stack entries. In this case, Arbno (P)
      --  can construct the following structure:

      --      +-------------+
      --      |             ^
      --      V             |
      --    +---+           |
      --    | S |---->      |
      --    +---+           |
      --      .             |
      --      .             |
      --    +---+           |
      --    | P |---------->+
      --    +---+

      --  The S (PC_Arbno_S) node matches null stacking a pointer to the
      --  pattern P. If a subsequent failure causes P to be matched and
      --  this match succeeds, then node A gets restacked to try another
      --  instance if needed by a subsequent failure.

      --  The node numbering of the constituent pattern P is not affected.
      --  The S node has a node number of P.Index + 1.

      --------------------------
      -- Arbno (complex case) --
      --------------------------

      --  A call to Arbno (P), where P can match null (or at least is not
      --  known to require a non-null string) and/or P requires pattern stack
      --  entries, constructs the following structure:

      --      +--------------------------+
      --      |                          ^
      --      V                          |
      --    +---+                        |
      --    | X |---->                   |
      --    +---+                        |
      --      .                          |
      --      .                          |
      --    +---+     +---+     +---+    |
      --    | E |---->| P |---->| Y |--->+
      --    +---+     +---+     +---+

      --  The node X (PC_Arbno_X) matches null, stacking a pointer to the
      --  E-P-X structure used to match one Arbno instance.

      --  Here E is the PC_R_Enter node which matches null and creates two
      --  stack entries. The first is a special entry whose node field is
      --  not used at all, and whose cursor field has the initial cursor.

      --  The second entry corresponds to a standard new region action. A
      --  PC_R_Remove node is stacked, whose cursor field is used to store
      --  the outer stack base, and the stack base is reset to point to
      --  this PC_R_Remove node. Then the pattern P is matched, and it can
      --  make history stack entries in the normal manner, so now the stack
      --  looks like:

      --     (stack entries made before assign pattern)

      --     (Special entry, node field not used,
      --      used only to save initial cursor)

      --     (PC_R_Remove entry, "cursor" value is (negative)  <-- Stack Base
      --      saved base value for the enclosing region)

      --     (stack entries made by matching P)

      --  If the match of P fails, then the PC_R_Remove entry is popped and
      --  it removes both itself and the special entry underneath it,
      --  restores the outer stack base, and signals failure.

      --  If the match of P succeeds, then node Y, the PC_Arbno_Y node, pops
      --  the inner region. There are two possibilities. If matching P left
      --  no stack entries, then all traces of the inner region can be removed.
      --  If there are stack entries, then we push an PC_Region_Replace stack
      --  entry whose "cursor" value is the inner stack base value, and then
      --  restore the outer stack base value, so the stack looks like:

      --     (stack entries made before assign pattern)

      --     (Special entry, node field not used,
      --      used only to save initial cursor)

      --     (PC_R_Remove entry, "cursor" value is (negative)
      --      saved base value for the enclosing region)

      --     (stack entries made by matching P)

      --     (PC_Region_Replace entry, "cursor" value is (negative)
      --      stack pointer value referencing the PC_R_Remove entry).

      --  Now that we have matched another instance of the Arbno pattern,
      --  we need to move to the successor. There are two cases. If the
      --  Arbno pattern matched null, then there is no point in seeking
      --  alternatives, since we would just match a whole bunch of nulls.
      --  In this case we look through the alternative node, and move
      --  directly to its successor (i.e. the successor of the Arbno
      --  pattern). If on the other hand a non-null string was matched,
      --  we simply follow the successor to the alternative node, which
      --  sets up for another possible match of the Arbno pattern.

      --  As noted in the section on stack checking, the stack count (and
      --  hence the stack check) for a pattern includes only one iteration
      --  of the Arbno pattern. To make sure that multiple iterations do not
      --  overflow the stack, the Arbno node saves the stack count required
      --  by a single iteration, and the Concat function increments this to
      --  include stack entries required by any successor. The PC_Arbno_Y
      --  node uses this count to ensure that sufficient stack remains
      --  before proceeding after matching each new instance.

      --  The node numbering of the constituent pattern P is not affected.
      --  Where N is the number of nodes in P, the Y node is numbered N + 1,
      --  the E node is N + 2, and the X node is N + 3.

      ----------------------
      -- Assign Immediate --
      ----------------------

      --  Immediate assignment (P * V) constructs the following structure

      --    +---+     +---+     +---+
      --    | E |---->| P |---->| A |---->
      --    +---+     +---+     +---+

      --  Here E is the PC_R_Enter node which matches null and creates two
      --  stack entries. The first is a special entry whose node field is
      --  not used at all, and whose cursor field has the initial cursor.

      --  The second entry corresponds to a standard new region action. A
      --  PC_R_Remove node is stacked, whose cursor field is used to store
      --  the outer stack base, and the stack base is reset to point to
      --  this PC_R_Remove node. Then the pattern P is matched, and it can
      --  make history stack entries in the normal manner, so now the stack
      --  looks like:

      --     (stack entries made before assign pattern)

      --     (Special entry, node field not used,
      --      used only to save initial cursor)

      --     (PC_R_Remove entry, "cursor" value is (negative)  <-- Stack Base
      --      saved base value for the enclosing region)

      --     (stack entries made by matching P)

      --  If the match of P fails, then the PC_R_Remove entry is popped
      --  and it removes both itself and the special entry underneath it,
      --  restores the outer stack base, and signals failure.

      --  If the match of P succeeds, then node A, which is the actual
      --  PC_Assign_Imm node, executes the assignment (using the stack
      --  base to locate the entry with the saved starting cursor value),
      --  and the pops the inner region. There are two possibilities, if
      --  matching P left no stack entries, then all traces of the inner
      --  region can be removed. If there are stack entries, then we push
      --  an PC_Region_Replace stack entry whose "cursor" value is the
      --  inner stack base value, and then restore the outer stack base
      --  value, so the stack looks like:

      --     (stack entries made before assign pattern)

      --     (Special entry, node field not used,
      --      used only to save initial cursor)

      --     (PC_R_Remove entry, "cursor" value is (negative)
      --      saved base value for the enclosing region)

      --     (stack entries made by matching P)

      --     (PC_Region_Replace entry, "cursor" value is the (negative)
      --      stack pointer value referencing the PC_R_Remove entry).

      --  If a subsequent failure occurs, the PC_Region_Replace node restores
      --  the inner stack base value and signals failure to explore rematches
      --  of the pattern P.

      --  The node numbering of the constituent pattern P is not affected.
      --  Where N is the number of nodes in P, the A node is numbered N + 1,
      --  and the E node is N + 2.

      ---------------------
      -- Assign On Match --
      ---------------------

      --  The assign on match (**) pattern is quite similar to the assign
      --  immediate pattern, except that the actual assignment has to be
      --  delayed. The following structure is constructed:

      --    +---+     +---+     +---+
      --    | E |---->| P |---->| A |---->
      --    +---+     +---+     +---+

      --  The operation of this pattern is identical to that described above
      --  for deferred assignment, up to the point where P has been matched.

      --  The A node, which is the PC_Assign_OnM node first pushes a
      --  PC_Assign node onto the history stack. This node saves the ending
      --  cursor and acts as a flag for the final assignment, as further
      --  described below.

      --  It then stores a pointer to itself in the special entry node field.
      --  This was otherwise unused, and is now used to retrive the address
      --  of the variable to be assigned at the end of the pattern.

      --  After that the inner region is terminated in the usual manner,
      --  by stacking a PC_R_Restore entry as described for the assign
      --  immediate case. Note that the optimization of completely
      --  removing the inner region does not happen in this case, since
      --  we have at least one stack entry (the PC_Assign one we just made).
      --  The stack now looks like:

      --     (stack entries made before assign pattern)

      --     (Special entry, node points to copy of
      --      the PC_Assign_OnM node, and the
      --      cursor field saves the initial cursor).

      --     (PC_R_Remove entry, "cursor" value is (negative)
      --      saved base value for the enclosing region)

      --     (stack entries made by matching P)

      --     (PC_Assign entry, saves final cursor)

      --     (PC_Region_Replace entry, "cursor" value is (negative)
      --      stack pointer value referencing the PC_R_Remove entry).

      --  If a subsequent failure causes the PC_Assign node to execute it
      --  simply removes itself and propagates the failure.

      --  If the match succeeds, then the history stack is scanned for
      --  PC_Assign nodes, and the assignments are executed (examination
      --  of the above diagram will show that all the necessary data is
      --  at hand for the assignment).

      --  To optimize the common case where no assign-on-match operations
      --  are present, a global flag Assign_OnM is maintained which is
      --  initialize to False, and gets set True as part of the execution
      --  of the PC_Assign_OnM node. The scan of the history stack for
      --  PC_Assign entries is done only if this flag is set.

      --  The node numbering of the constituent pattern P is not affected.
      --  Where N is the number of nodes in P, the A node is numbered N + 1,
      --  and the E node is N + 2.

      ---------
      -- Bal --
      ---------

      --  Bal builds a single node:

      --    +---+
      --    | B |---->
      --    +---+

      --  The node B is the PC_Bal node which matches a parentheses balanced
      --  string, starting at the current cursor position. It then updates
      --  the cursor past this matched string, and stacks a pointer to itself
      --  with this updated cursor value on the history stack, to extend the
      --  matched string on a subequent failure.

      --  Since this is a single node it is numbered 1 (the reason we include
      --  it in the compound patterns section is that it backtracks).

      ------------
      -- BreakX --
      ------------

      --  BreakX builds the structure

      --    +---+     +---+
      --    | B |---->| A |---->
      --    +---+     +---+
      --      ^         .
      --      |         .
      --      |       +---+
      --      +<------| X |
      --              +---+

      --  Here the B node is the BreakX_xx node that performs a normal Break
      --  function. The A node is an alternative (PC_Alt) node that matches
      --  null, but stacks a pointer to node X (the PC_BreakX_X node) which
      --  extends the match one character (to eat up the previously detected
      --  break character), and then rematches the break.

      --  The B node is numbered 3, the alternative node is 1, and the X
      --  node is 2.

      -----------
      -- Fence --
      -----------

      --  Fence builds a single node:

      --    +---+
      --    | F |---->
      --    +---+

      --  The element F, PC_Fence,  matches null, and stacks a pointer to a
      --  PC_Cancel element which will abort the match on a subsequent failure.

      --  Since this is a single element it is numbered 1 (the reason we
      --  include it in the compound patterns section is that it backtracks).

      --------------------
      -- Fence Function --
      --------------------

      --  A call to the Fence function builds the structure:

      --    +---+     +---+     +---+
      --    | E |---->| P |---->| X |---->
      --    +---+     +---+     +---+

      --  Here E is the PC_R_Enter node which matches null and creates two
      --  stack entries. The first is a special entry which is not used at
      --  all in the fence case (it is present merely for uniformity with
      --  other cases of region enter operations).

      --  The second entry corresponds to a standard new region action. A
      --  PC_R_Remove node is stacked, whose cursor field is used to store
      --  the outer stack base, and the stack base is reset to point to
      --  this PC_R_Remove node. Then the pattern P is matched, and it can
      --  make history stack entries in the normal manner, so now the stack
      --  looks like:

      --     (stack entries made before fence pattern)

      --     (Special entry, not used at all)

      --     (PC_R_Remove entry, "cursor" value is (negative)  <-- Stack Base
      --      saved base value for the enclosing region)

      --     (stack entries made by matching P)

      --  If the match of P fails, then the PC_R_Remove entry is popped
      --  and it removes both itself and the special entry underneath it,
      --  restores the outer stack base, and signals failure.

      --  If the match of P succeeds, then node X, the PC_Fence_X node, gets
      --  control. One might be tempted to think that at this point, the
      --  history stack entries made by matching P can just be removed since
      --  they certainly are not going to be used for rematching (that is
      --  whole point of Fence after all!) However, this is wrong, because
      --  it would result in the loss of possible assign-on-match entries
      --  for deferred pattern assignments.

      --  Instead what we do is to make a special entry whose node references
      --  PC_Fence_Y, and whose cursor saves the inner stack base value, i.e.
      --  the pointer to the PC_R_Remove entry. Then the outer stack base
      --  pointer is restored, so the stack looks like:

      --     (stack entries made before assign pattern)

      --     (Special entry, not used at all)

      --     (PC_R_Remove entry, "cursor" value is (negative)
      --      saved base value for the enclosing region)

      --     (stack entries made by matching P)

      --     (PC_Fence_Y entry, "cursor" value is (negative) stack
      --      pointer value referencing the PC_R_Remove entry).

      --  If a subsequent failure occurs, then the PC_Fence_Y entry removes
      --  the entire inner region, including all entries made by matching P,
      --  and alternatives prior to the Fence pattern are sought.

      --  The node numbering of the constituent pattern P is not affected.
      --  Where N is the number of nodes in P, the X node is numbered N + 1,
      --  and the E node is N + 2.

      -------------
      -- Succeed --
      -------------

      --  Succeed builds a single node:

      --    +---+
      --    | S |---->
      --    +---+

      --  The node S is the PC_Succeed node which matches null, and stacks
      --  a pointer to itself on the history stack, so that a subsequent
      --  failure repeats the same match.

      --  Since this is a single node it is numbered 1 (the reason we include
      --  it in the compound patterns section is that it backtracks).

      ---------------------
      -- Write Immediate --
      ---------------------

      --  The structure built for a write immediate operation (P * F, where
      --  F is a file access value) is:

      --    +---+     +---+     +---+
      --    | E |---->| P |---->| W |---->
      --    +---+     +---+     +---+

      --  Here E is the PC_R_Enter node and W is the PC_Write_Imm node. The
      --  handling is identical to that described above for Assign Immediate,
      --  except that at the point where a successful match occurs, the matched
      --  substring is written to the referenced file.

      --  The node numbering of the constituent pattern P is not affected.
      --  Where N is the number of nodes in P, the W node is numbered N + 1,
      --  and the E node is N + 2.

      --------------------
      -- Write On Match --
      --------------------

      --  The structure built for a write on match operation (P ** F, where
      --  F is a file access value) is:

      --    +---+     +---+     +---+
      --    | E |---->| P |---->| W |---->
      --    +---+     +---+     +---+

      --  Here E is the PC_R_Enter node and W is the PC_Write_OnM node. The
      --  handling is identical to that described above for Assign On Match,
      --  except that at the point where a successful match has completed,
      --  the matched substring is written to the referenced file.

      --  The node numbering of the constituent pattern P is not affected.
      --  Where N is the number of nodes in P, the W node is numbered N + 1,
      --  and the E node is N + 2.
   -----------------------
   -- Constant Patterns --
   -----------------------

   --  The following pattern elements are referenced only from the pattern
   --  history stack. In each case the processing for the pattern element
   --  results in pattern match abort, or futher failure, so there is no
   --  need for a successor and no need for a node number

   CP_Assign    : aliased PE := (PC_Assign,    0, N);
   CP_Cancel    : aliased PE := (PC_Cancel,    0, N);
   CP_Fence_Y   : aliased PE := (PC_Fence_Y,   0, N);
   CP_R_Remove  : aliased PE := (PC_R_Remove,  0, N);
   CP_R_Restore : aliased PE := (PC_R_Restore, 0, N);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Alternate (L, R : PE_Ptr) return PE_Ptr;
   function "or"      (L, R : PE_Ptr) return PE_Ptr renames Alternate;
   --  Build pattern structure corresponding to the alternation of L, R.
   --  (i.e. try to match L, and if that fails, try to match R).

   function Arbno_Simple (P : PE_Ptr) return PE_Ptr;
   --  Build simple Arbno pattern, P is a pattern that is guaranteed to
   --  match at least one character if it succeeds and to require no
   --  stack entries under all circumstances. The result returned is
   --  a simple Arbno structure as previously described.

   function Bracket (E, P, A : PE_Ptr) return PE_Ptr;
   --  Given two single node pattern elements E and A, and a (possible
   --  complex) pattern P, construct the concatenation E-->P-->A and
   --  return a pointer to E. The concatenation does not affect the
   --  node numbering in P. A has a number one higher than the maximum
   --  number in P, and E has a number two higher than the maximum
   --  number in P (see for example the Assign_Immediate structure to
   --  understand a typical use of this function).

   function BreakX_Make (B : PE_Ptr) return Pattern;
   --  Given a pattern element for a Break patternx, returns the
   --  corresponding BreakX compound pattern structure.

   function Concat (L, R : PE_Ptr; Incr : Natural) return PE_Ptr;
   --  Creates a pattern eelement that represents a concatenation of the
   --  two given pattern elements (i.e. the pattern L followed by R).
   --  The result returned is always the same as L, but the pattern
   --  referenced by L is modified to have R as a successor. This
   --  procedure does not copy L or R, so if a copy is required, it
   --  is the responsibility of the caller. The Incr parameter is an
   --  amount to be added to the Nat field of any P_Arbno_Y node that is
   --  in the left operand, it represents the additional stack space
   --  required by the right operand.

   function "&" (L, R : PE_Ptr) return PE_Ptr;
   pragma Inline ("&");
   --  Equivalent to Concat (L, R, 0)

   function C_To_PE (C : PChar) return PE_Ptr;
   --  Given a character, constructs a pattern element that matches
   --  the single character.

   function Copy (P : PE_Ptr) return PE_Ptr;
   --  Creates a copy of the pattern element referenced by the given
   --  pattern element reference. This is a deep copy, which means that
   --  it follows the Next and Alt pointers.

   function Image (P : PE_Ptr) return String;
   --  Returns the image of the address of the referenced pattern element.
   --  This is equivalent to Image (To_Address (P));

   function Is_In (C : Character; Str : String) return Boolean;
   pragma Inline (Is_In);
   --  Determines if the character C is in string Str.

   procedure Logic_Error;
   --  Called to raise Program_Error with an appropriate message if an
   --  internal logic error is detected.

   function Str_BF (A : Boolean_Func)   return String;
   function Str_FP (A : File_Ptr)       return String;
   function Str_NF (A : Natural_Func)   return String;
   function Str_NP (A : Natural_Ptr)    return String;
   function Str_PP (A : Pattern_Ptr)    return String;
   function Str_VF (A : VString_Func)   return String;
   function Str_VP (A : VString_Ptr)    return String;
   --  These are debugging routines, which return a representation of the
   --  given access value (they are called only by Image and Dump)

   procedure Set_Successor (Pat : PE_Ptr; Succ : PE_Ptr);
   --  Adjusts all EOP pointers in Pat to point to Succ. No other changes
   --  are made. In particular, Succ is unchanged, and no index numbers
   --  are modified. Note that Pat may not be equal to EOP on entry.

   function S_To_PE (Str : PString) return PE_Ptr;
   --  Given a string, constructs a pattern element that matches the string

   procedure Uninitialized_Pattern;
   pragma No_Return (Uninitialized_Pattern);
   --  Called to raise Program_Error with an appropriate error message if
   --  an uninitialized pattern is used in any pattern construction or
   --  pattern matching operation.

   procedure XMatch
     (Subject : String;
      Pat_P   : PE_Ptr;
      Pat_S   : Natural;
      Start   : out Natural;
      Stop    : out Natural);
   --  This is the common pattern match routine. It is passed a string and
   --  a pattern, and it indicates success or failure, and on success the
   --  section of the string matched. It does not perform any assignments
   --  to the subject string, so pattern replacement is for the caller.
   --
   --  Subject The subject string. The lower bound is always one. In the
   --          Match procedures, it is fine to use strings whose lower bound
   --          is not one, but we perform a one time conversion before the
   --          call to XMatch, so that XMatch does not have to be bothered
   --          with strange lower bounds.
   --
   --  Pat_P   Points to initial pattern element of pattern to be matched
   --
   --  Pat_S   Maximum required stack entries for pattern to be matched
   --
   --  Start   If match is successful, starting index of matched section.
   --          This value is always non-zero. A value of zero is used to
   --          indicate a failed match.
   --
   --  Stop    If match is successful, ending index of matched section.
   --          This can be zero if we match the null string at the start,
   --          in which case Start is set to zero, and Stop to one. If the
   --          Match fails, then the contents of Stop is undefined.

   procedure XMatchD
     (Subject : String;
      Pat_P   : PE_Ptr;
      Pat_S   : Natural;
      Start   : out Natural;
      Stop    : out Natural);
   --  Identical in all respects to XMatch, except that trace information is
   --  output on Standard_Output during execution of the match. This is the
   --  version that is called if the original Match call has Debug => True.

   ---------
   -- "&" --
   ---------

   function "&" (L : PString; R : Pattern) return Pattern is
   begin
      return (AFC with R.Stk, Concat (S_To_PE (L), Copy (R.P), R.Stk));
   end "&";

   function "&" (L : Pattern; R : PString) return Pattern is
   begin
      return (AFC with L.Stk, Concat (Copy (L.P), S_To_PE (R), 0));
   end "&";

   function "&" (L : PChar; R : Pattern) return Pattern is
   begin
      return (AFC with R.Stk, Concat (C_To_PE (L), Copy (R.P), R.Stk));
   end "&";

   function "&" (L : Pattern; R : PChar) return Pattern is
   begin
      return (AFC with L.Stk, Concat (Copy (L.P), C_To_PE (R), 0));
   end "&";

   function "&" (L : Pattern; R : Pattern) return Pattern is
   begin
      return (AFC with L.Stk + R.Stk, Concat (Copy (L.P), Copy (R.P), R.Stk));
   end "&";

   function "&" (L, R : PE_Ptr) return PE_Ptr is
   begin
      return Concat (L, R, 0);
   end "&";

   ---------
   -- "*" --
   ---------

   --  Assign immediate

   --    +---+     +---+     +---+
   --    | E |---->| P |---->| A |---->
   --    +---+     +---+     +---+

   --  The node numbering of the constituent pattern P is not affected.
   --  Where N is the number of nodes in P, the A node is numbered N + 1,
   --  and the E node is N + 2.

   function "*" (P : Pattern; Var : VString_Var) return Pattern is
      Pat : constant PE_Ptr := Copy (P.P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,    0, EOP);
      A   : constant PE_Ptr :=
              new PE'(PC_Assign_Imm, 0, EOP, Var'Unrestricted_Access);

   begin
      return (AFC with P.Stk + 3, Bracket (E, Pat, A));
   end "*";

   function "*" (P : PString; Var : VString_Var) return Pattern is
      Pat : constant PE_Ptr := S_To_PE (P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,    0, EOP);
      A   : constant PE_Ptr :=
              new PE'(PC_Assign_Imm, 0, EOP, Var'Unrestricted_Access);

   begin
      return (AFC with 3, Bracket (E, Pat, A));
   end "*";

   function "*" (P : PChar; Var : VString_Var) return Pattern is
      Pat : constant PE_Ptr := C_To_PE (P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,    0, EOP);
      A   : constant PE_Ptr :=
              new PE'(PC_Assign_Imm, 0, EOP, Var'Unrestricted_Access);

   begin
      return (AFC with 3, Bracket (E, Pat, A));
   end "*";

   --  Write immediate

   --    +---+     +---+     +---+
   --    | E |---->| P |---->| W |---->
   --    +---+     +---+     +---+

   --  The node numbering of the constituent pattern P is not affected.
   --  Where N is the number of nodes in P, the W node is numbered N + 1,
   --  and the E node is N + 2.

   function "*" (P : Pattern; Fil : File_Access) return Pattern is
      Pat : constant PE_Ptr := Copy (P.P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,   0, EOP);
      W   : constant PE_Ptr := new PE'(PC_Write_Imm, 0, EOP, Fil);

   begin
      return (AFC with 3, Bracket (E, Pat, W));
   end "*";

   function "*" (P : PString; Fil : File_Access) return Pattern is
      Pat : constant PE_Ptr := S_To_PE (P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,   0, EOP);
      W   : constant PE_Ptr := new PE'(PC_Write_Imm, 0, EOP, Fil);

   begin
      return (AFC with 3, Bracket (E, Pat, W));
   end "*";

   function "*" (P : PChar; Fil : File_Access) return Pattern is
      Pat : constant PE_Ptr := C_To_PE (P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,   0, EOP);
      W   : constant PE_Ptr := new PE'(PC_Write_Imm, 0, EOP, Fil);

   begin
      return (AFC with 3, Bracket (E, Pat, W));
   end "*";

   ----------
   -- "**" --
   ----------

   --  Assign on match

   --    +---+     +---+     +---+
   --    | E |---->| P |---->| A |---->
   --    +---+     +---+     +---+

   --  The node numbering of the constituent pattern P is not affected.
   --  Where N is the number of nodes in P, the A node is numbered N + 1,
   --  and the E node is N + 2.

   function "**" (P : Pattern; Var : VString_Var) return Pattern is
      Pat : constant PE_Ptr := Copy (P.P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,    0, EOP);
      A   : constant PE_Ptr :=
              new PE'(PC_Assign_OnM, 0, EOP, Var'Unrestricted_Access);

   begin
      return (AFC with P.Stk + 3, Bracket (E, Pat, A));
   end "**";

   function "**" (P : PString; Var : VString_Var) return Pattern is
      Pat : constant PE_Ptr := S_To_PE (P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,    0, EOP);
      A   : constant PE_Ptr :=
              new PE'(PC_Assign_OnM, 0, EOP, Var'Unrestricted_Access);

   begin
      return (AFC with 3, Bracket (E, Pat, A));
   end "**";

   function "**" (P : PChar; Var : VString_Var) return Pattern is
      Pat : constant PE_Ptr := C_To_PE (P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,    0, EOP);
      A   : constant PE_Ptr :=
              new PE'(PC_Assign_OnM, 0, EOP, Var'Unrestricted_Access);

   begin
      return (AFC with 3, Bracket (E, Pat, A));
   end "**";

   --  Write on match

   --    +---+     +---+     +---+
   --    | E |---->| P |---->| W |---->
   --    +---+     +---+     +---+

   --  The node numbering of the constituent pattern P is not affected.
   --  Where N is the number of nodes in P, the W node is numbered N + 1,
   --  and the E node is N + 2.

   function "**" (P : Pattern; Fil : File_Access) return Pattern is
      Pat : constant PE_Ptr := Copy (P.P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,   0, EOP);
      W   : constant PE_Ptr := new PE'(PC_Write_OnM, 0, EOP, Fil);

   begin
      return (AFC with P.Stk + 3, Bracket (E, Pat, W));
   end "**";

   function "**" (P : PString; Fil : File_Access) return Pattern is
      Pat : constant PE_Ptr := S_To_PE (P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,   0, EOP);
      W   : constant PE_Ptr := new PE'(PC_Write_OnM, 0, EOP, Fil);

   begin
      return (AFC with 3, Bracket (E, Pat, W));
   end "**";

   function "**" (P : PChar; Fil : File_Access) return Pattern is
      Pat : constant PE_Ptr := C_To_PE (P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter,   0, EOP);
      W   : constant PE_Ptr := new PE'(PC_Write_OnM, 0, EOP, Fil);

   begin
      return (AFC with 3, Bracket (E, Pat, W));
   end "**";

   ---------
   -- "+" --
   ---------

   function "+" (Str : VString_Var) return Pattern is
   begin
      return
        (AFC with 0,
         new PE'(PC_String_VP, 1, EOP, Str'Unrestricted_Access));
   end "+";

   function "+" (Str : VString_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_String_VF, 1, EOP, Str));
   end "+";

   function "+" (P : Pattern_Var) return Pattern is
   begin
      return
        (AFC with 3,
         new PE'(PC_Rpat, 1, EOP, P'Unrestricted_Access));
   end "+";

   function "+" (P : Boolean_Func) return Pattern is
   begin
      return (AFC with 3, new PE'(PC_Pred_Func, 1, EOP, P));
   end "+";

   ----------
   -- "or" --
   ----------

   function "or" (L : PString; R : Pattern) return Pattern is
   begin
      return (AFC with R.Stk + 1, S_To_PE (L) or Copy (R.P));
   end "or";

   function "or" (L : Pattern; R : PString) return Pattern is
   begin
      return (AFC with L.Stk + 1, Copy (L.P) or S_To_PE (R));
   end "or";

   function "or" (L : PString; R : PString) return Pattern is
   begin
      return (AFC with 1, S_To_PE (L) or S_To_PE (R));
   end "or";

   function "or" (L : Pattern; R : Pattern) return Pattern is
   begin
      return (AFC with
                Natural'Max (L.Stk, R.Stk) + 1, Copy (L.P) or Copy (R.P));
   end "or";

   function "or" (L : PChar;   R : Pattern) return Pattern is
   begin
      return (AFC with 1, C_To_PE (L) or Copy (R.P));
   end "or";

   function "or" (L : Pattern; R : PChar) return Pattern is
   begin
      return (AFC with 1, Copy (L.P) or C_To_PE (R));
   end "or";

   function "or" (L : PChar;   R : PChar) return Pattern is
   begin
      return (AFC with 1, C_To_PE (L) or C_To_PE (R));
   end "or";

   function "or" (L : PString; R : PChar) return Pattern is
   begin
      return (AFC with 1, S_To_PE (L) or C_To_PE (R));
   end "or";

   function "or" (L : PChar;   R : PString) return Pattern is
   begin
      return (AFC with 1, C_To_PE (L) or S_To_PE (R));
   end "or";

   ------------
   -- Adjust --
   ------------

   --  No two patterns share the same pattern elements, so the adjust
   --  procedure for a Pattern assignment must do a deep copy of the
   --  pattern element structure.

   procedure Adjust (Object : in out Pattern) is
   begin
      Object.P := Copy (Object.P);
   end Adjust;

   ---------------
   -- Alternate --
   ---------------

   function Alternate (L, R : PE_Ptr) return PE_Ptr is
   begin
      --  If the left pattern is null, then we just add the alternation
      --  node with an index one greater than the right hand pattern.

      if L = EOP then
         return new PE'(PC_Alt, R.Index + 1, EOP, R);

      --  If the left pattern is non-null, then build a reference vector
      --  for its elements, and adjust their index values to acccomodate
      --  the right hand elements. Then add the alternation node.

      else
         declare
            Refs : Ref_Array (1 .. L.Index);

         begin
            Build_Ref_Array (L, Refs);

            for J in Refs'Range loop
               Refs (J).Index := Refs (J).Index + R.Index;
            end loop;
         end;

         return new PE'(PC_Alt, L.Index + 1, L, R);
      end if;
   end Alternate;

   ---------
   -- Any --
   ---------

   function Any (Str : String) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Any_CS, 1, EOP, To_Set (Str)));
   end Any;

   function Any (Str : VString) return Pattern is
   begin
      return Any (S (Str));
   end Any;

   function Any (Str : Character) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Any_CH, 1, EOP, Str));
   end Any;

   function Any (Str : Character_Set) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Any_CS, 1, EOP, Str));
   end Any;

   function Any (Str : access VString) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Any_VP, 1, EOP, VString_Ptr (Str)));
   end Any;

   function Any (Str : VString_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Any_VF, 1, EOP, Str));
   end Any;

   ---------
   -- Arb --
   ---------

   --    +---+
   --    | X |---->
   --    +---+
   --      .
   --      .
   --    +---+
   --    | Y |---->
   --    +---+

   --  The PC_Arb_X element is numbered 2, and the PC_Arb_Y element is 1.

   function Arb return Pattern is
      Y : constant PE_Ptr := new PE'(PC_Arb_Y, 1, EOP);
      X : constant PE_Ptr := new PE'(PC_Arb_X, 2, EOP, Y);

   begin
      return (AFC with 1, X);
   end Arb;

   -----------
   -- Arbno --
   -----------

   function Arbno (P : PString) return Pattern is
   begin
      if P'Length = 0 then
         return (AFC with 0, EOP);

      else
         return (AFC with 0, Arbno_Simple (S_To_PE (P)));
      end if;
   end Arbno;

   function Arbno (P : PChar) return Pattern is
   begin
      return (AFC with 0, Arbno_Simple (C_To_PE (P)));
   end Arbno;

   function Arbno (P : Pattern) return Pattern is
      Pat : constant PE_Ptr := Copy (P.P);

   begin
      if P.Stk = 0
        and then OK_For_Simple_Arbno (Pat.Pcode)
      then
         return (AFC with 0, Arbno_Simple (Pat));
      end if;

      --  This is the complex case, either the pattern makes stack entries
      --  or it is possible for the pattern to match the null string (more
      --  accurately, we don't know that this is not the case).

      --      +--------------------------+
      --      |                          ^
      --      V                          |
      --    +---+                        |
      --    | X |---->                   |
      --    +---+                        |
      --      .                          |
      --      .                          |
      --    +---+     +---+     +---+    |
      --    | E |---->| P |---->| Y |--->+
      --    +---+     +---+     +---+

      --  The node numbering of the constituent pattern P is not affected.
      --  Where N is the number of nodes in P, the Y node is numbered N + 1,
      --  the E node is N + 2, and the X node is N + 3.

      declare
         E   : constant PE_Ptr := new PE'(PC_R_Enter, 0, EOP);
         X   : constant PE_Ptr := new PE'(PC_Arbno_X, 0, EOP, E);
         Y   : constant PE_Ptr := new PE'(PC_Arbno_Y, 0, X,   P.Stk + 3);
         EPY : constant PE_Ptr := Bracket (E, Pat, Y);

      begin
         X.Alt := EPY;
         X.Index := EPY.Index + 1;
         return (AFC with P.Stk + 3, X);
      end;
   end Arbno;

   ------------------
   -- Arbno_Simple --
   ------------------

      --      +-------------+
      --      |             ^
      --      V             |
      --    +---+           |
      --    | S |---->      |
      --    +---+           |
      --      .             |
      --      .             |
      --    +---+           |
      --    | P |---------->+
      --    +---+

   --  The node numbering of the constituent pattern P is not affected.
   --  The S node has a node number of P.Index + 1.

   --  Note that we know that P cannot be EOP, because a null pattern
   --  does not meet the requirements for simple Arbno.

   function Arbno_Simple (P : PE_Ptr) return PE_Ptr is
      S : constant PE_Ptr := new PE'(PC_Arbno_S, P.Index + 1, EOP, P);

   begin
      Set_Successor (P, S);
      return S;
   end Arbno_Simple;

   ---------
   -- Bal --
   ---------

   function Bal return Pattern is
   begin
      return (AFC with 1, new PE'(PC_Bal, 1, EOP));
   end Bal;

   -------------
   -- Bracket --
   -------------

   function Bracket (E, P, A : PE_Ptr) return PE_Ptr is
   begin
      if P = EOP then
         E.Pthen := A;
         E.Index := 2;
         A.Index := 1;

      else
         E.Pthen := P;
         Set_Successor (P, A);
         E.Index := P.Index + 2;
         A.Index := P.Index + 1;
      end if;

      return E;
   end Bracket;

   -----------
   -- Break --
   -----------

   function Break (Str : String) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Break_CS, 1, EOP, To_Set (Str)));
   end Break;

   function Break (Str : VString) return Pattern is
   begin
      return Break (S (Str));
   end Break;

   function Break (Str : Character) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Break_CH, 1, EOP, Str));
   end Break;

   function Break (Str : Character_Set) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Break_CS, 1, EOP, Str));
   end Break;

   function Break (Str : access VString) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Break_VP, 1, EOP, VString_Ptr (Str)));
   end Break;

   function Break (Str : VString_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Break_VF, 1, EOP, Str));
   end Break;

   ------------
   -- BreakX --
   ------------

   function BreakX (Str : String) return Pattern is
   begin
      return BreakX_Make (new PE'(PC_BreakX_CS, 3, N, To_Set (Str)));
   end BreakX;

   function BreakX (Str : VString) return Pattern is
   begin
      return BreakX (S (Str));
   end BreakX;

   function BreakX (Str : Character) return Pattern is
   begin
      return BreakX_Make (new PE'(PC_BreakX_CH, 3, N, Str));
   end BreakX;

   function BreakX (Str : Character_Set) return Pattern is
   begin
      return BreakX_Make (new PE'(PC_BreakX_CS, 3, N, Str));
   end BreakX;

   function BreakX (Str : access VString) return Pattern is
   begin
      return BreakX_Make (new PE'(PC_BreakX_VP, 3, N, VString_Ptr (Str)));
   end BreakX;

   function BreakX (Str : VString_Func) return Pattern is
   begin
      return BreakX_Make (new PE'(PC_BreakX_VF, 3, N, Str));
   end BreakX;

   -----------------
   -- BreakX_Make --
   -----------------

   --    +---+     +---+
   --    | B |---->| A |---->
   --    +---+     +---+
   --      ^         .
   --      |         .
   --      |       +---+
   --      +<------| X |
   --              +---+

   --  The B node is numbered 3, the alternative node is 1, and the X
   --  node is 2.

   function BreakX_Make (B : PE_Ptr) return Pattern is
      X : constant PE_Ptr := new PE'(PC_BreakX_X, 2, B);
      A : constant PE_Ptr := new PE'(PC_Alt,      1, EOP, X);

   begin
      B.Pthen := A;
      return (AFC with 2, B);
   end BreakX_Make;

   ---------------------
   -- Build_Ref_Array --
   ---------------------

   procedure Build_Ref_Array (E : PE_Ptr; RA : out Ref_Array) is

      procedure Record_PE (E : PE_Ptr);
      --  Record given pattern element if not already recorded in RA,
      --  and also record any referenced pattern elements recursively.

      procedure Record_PE (E : PE_Ptr) is
      begin
         PutD ("  Record_PE called with PE_Ptr = " & Image (E));

         if E = EOP or else RA (E.Index) /= null then
            Put_LineD (", nothing to do");
            return;

         else
            Put_LineD (", recording" & IndexT'Image (E.Index));
            RA (E.Index) := E;
            Record_PE (E.Pthen);

            if E.Pcode in PC_Has_Alt then
               Record_PE (E.Alt);
            end if;
         end if;
      end Record_PE;

   --  Start of processing for Build_Ref_Array

   begin
      New_LineD;
      Put_LineD ("Entering Build_Ref_Array");
      Record_PE (E);
      New_LineD;
   end Build_Ref_Array;

   -------------
   -- C_To_PE --
   -------------

   function C_To_PE (C : PChar) return PE_Ptr is
   begin
      return new PE'(PC_Char, 1, EOP, C);
   end C_To_PE;

   ------------
   -- Cancel --
   ------------

   function Cancel return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Cancel, 1, EOP));
   end Cancel;

   ------------
   -- Concat --
   ------------

   --  Concat needs to traverse the left operand performing the following
   --  set of fixups:

   --    a) Any successor pointers (Pthen fields) that are set to EOP are
   --       reset to point to the second operand.

   --    b) Any PC_Arbno_Y node has its stack count field incremented
   --       by the parameter Incr provided for this purpose.

   --    d) Num fields of all pattern elements in the left operand are
   --       adjusted to include the elements of the right operand.

   --  Note: we do not use Set_Successor in the processing for Concat, since
   --  there is no point in doing two traversals, we may as well do everything
   --  at the same time.

   function Concat (L, R : PE_Ptr; Incr : Natural) return PE_Ptr is
   begin
      if L = EOP then
         return R;

      elsif R = EOP then
         return L;

      else
         declare
            Refs : Ref_Array (1 .. L.Index);
            --  We build a reference array for L whose N'th element points to
            --  the pattern element of L whose original Index value is N.

            P : PE_Ptr;

         begin
            Build_Ref_Array (L, Refs);

            for J in Refs'Range loop
               P := Refs (J);

               P.Index := P.Index + R.Index;

               if P.Pcode = PC_Arbno_Y then
                  P.Nat := P.Nat + Incr;
               end if;

               if P.Pthen = EOP then
                  P.Pthen := R;
               end if;

               if P.Pcode in PC_Has_Alt and then P.Alt = EOP then
                  P.Alt := R;
               end if;
            end loop;
         end;

         return L;
      end if;
   end Concat;

   ----------
   -- Copy --
   ----------

   function Copy (P : PE_Ptr) return PE_Ptr is
   begin
      if P = null then
         Uninitialized_Pattern;

      else
         declare
            Refs : Ref_Array (1 .. P.Index);
            --  References to elements in P, indexed by Index field

            Copy : Ref_Array (1 .. P.Index);
            --  Holds copies of elements of P, indexed by Index field.

            E : PE_Ptr;

         begin
            Build_Ref_Array (P, Refs);

            --  Now copy all nodes

            for J in Refs'Range loop
               Copy (J) := new PE'(Refs (J).all);
            end loop;

            --  Adjust all internal references

            for J in Copy'Range loop
               E := Copy (J);

               --  Adjust successor pointer to point to copy

               if E.Pthen /= EOP then
                  E.Pthen := Copy (E.Pthen.Index);
               end if;

               --  Adjust Alt pointer if there is one to point to copy

               if E.Pcode in PC_Has_Alt and then E.Alt /= EOP then
                  E.Alt := Copy (E.Alt.Index);
               end if;

               --  Copy referenced string

               if E.Pcode = PC_String then
                  E.Str := new String'(E.Str.all);
               end if;
            end loop;

            return Copy (P.Index);
         end;
      end if;
   end Copy;

   ----------
   -- Dump --
   ----------

   procedure Dump (P : Pattern) is

      subtype Count is Ada.Text_IO.Count;
      Scol : Count;
      --  Used to keep track of column in dump output

      Refs : Ref_Array (1 .. P.P.Index);
      --  We build a reference array whose N'th element points to the
      --  pattern element whose Index value is N.

      Cols : Natural := 2;
      --  Number of columns used for pattern numbers, minimum is 2

      E : PE_Ptr;

      procedure Write_Node_Id (E : PE_Ptr);
      --  Writes out a string identifying the given pattern element.

      procedure Write_Node_Id (E : PE_Ptr) is
      begin
         if E = EOP then
            Put ("EOP");

            for J in 4 .. Cols loop
               Put (' ');
            end loop;

         else
            declare
               Str : String (1 .. Cols);
               N   : Natural := Natural (E.Index);

            begin
               Put ("#");

               for J in reverse Str'Range loop
                  Str (J) := Character'Val (48 + N mod 10);
                  N := N / 10;
               end loop;

               Put (Str);
            end;
         end if;
      end Write_Node_Id;

   begin
      New_Line;
      Put ("Pattern Dump Output (pattern at " &
           Image (P'Address) &
           ", S = " & Natural'Image (P.Stk) & ')');

      Scol := Col;
      New_Line;

      while Col < Scol loop
         Put ('-');
      end loop;

      New_Line;

      --  If uninitialized pattern, dump line and we are done

      if P.P = null then
         Put_Line ("Uninitialized pattern value");
         return;
      end if;

      --  If null pattern, just dump it and we are all done

      if P.P = EOP then
         Put_Line ("EOP (null pattern)");
         return;
      end if;

      Build_Ref_Array (P.P, Refs);

      --  Set number of columns required for node numbers

      while 10 ** Cols - 1 < Integer (P.P.Index) loop
         Cols := Cols + 1;
      end loop;

      --  Now dump the nodes in reverse sequence. We output them in reverse
      --  sequence since this corresponds to the natural order used to
      --  construct the patterns.

      for J in reverse Refs'Range loop
         E := Refs (J);
         Write_Node_Id (E);
         Set_Col (Count (Cols) + 4);
         Put (Image (E));
         Put ("  ");
         Put (Pattern_Code'Image (E.Pcode));
         Put ("  ");
         Set_Col (21 + Count (Cols) + Address_Image_Length);
         Write_Node_Id (E.Pthen);
         Set_Col (24 + 2 * Count (Cols) + Address_Image_Length);

         case E.Pcode is

            when PC_Alt     |
                 PC_Arb_X   |
                 PC_Arbno_S |
                 PC_Arbno_X =>
               Write_Node_Id (E.Alt);

            when PC_Rpat =>
               Put (Str_PP (E.PP));

            when PC_Pred_Func =>
               Put (Str_BF (E.BF));

            when PC_Assign_Imm |
                 PC_Assign_OnM |
                 PC_Any_VP     |
                 PC_Break_VP   |
                 PC_BreakX_VP  |
                 PC_NotAny_VP  |
                 PC_NSpan_VP   |
                 PC_Span_VP    |
                 PC_String_VP  =>
               Put (Str_VP (E.VP));

            when PC_Write_Imm  |
                 PC_Write_OnM =>
               Put (Str_FP (E.FP));

            when PC_String =>
               Put (Image (E.Str.all));

            when PC_String_2 =>
               Put (Image (E.Str2));

            when PC_String_3 =>
               Put (Image (E.Str3));

            when PC_String_4 =>
               Put (Image (E.Str4));

            when PC_String_5 =>
               Put (Image (E.Str5));

            when PC_String_6 =>
               Put (Image (E.Str6));

            when PC_Setcur =>
               Put (Str_NP (E.Var));

            when PC_Any_CH      |
                 PC_Break_CH    |
                 PC_BreakX_CH   |
                 PC_Char        |
                 PC_NotAny_CH   |
                 PC_NSpan_CH    |
                 PC_Span_CH     =>
               Put (''' & E.Char & ''');

            when PC_Any_CS      |
                 PC_Break_CS    |
                 PC_BreakX_CS   |
                 PC_NotAny_CS   |
                 PC_NSpan_CS    |
                 PC_Span_CS     =>
               Put ('"' & To_Sequence (E.CS) & '"');

            when PC_Arbno_Y     |
                 PC_Len_Nat     |
                 PC_Pos_Nat     |
                 PC_RPos_Nat    |
                 PC_RTab_Nat    |
                 PC_Tab_Nat     =>
               Put (S (E.Nat));

            when PC_Pos_NF      |
                 PC_Len_NF      |
                 PC_RPos_NF     |
                 PC_RTab_NF     |
                 PC_Tab_NF      =>
               Put (Str_NF (E.NF));

            when PC_Pos_NP      |
                 PC_Len_NP      |
                 PC_RPos_NP     |
                 PC_RTab_NP     |
                 PC_Tab_NP      =>
               Put (Str_NP (E.NP));

            when PC_Any_VF      |
                 PC_Break_VF    |
                 PC_BreakX_VF   |
                 PC_NotAny_VF   |
                 PC_NSpan_VF    |
                 PC_Span_VF     |
                 PC_String_VF   =>
               Put (Str_VF (E.VF));

            when others => null;

         end case;

         New_Line;
      end loop;

      New_Line;
   end Dump;

   ----------
   -- Fail --
   ----------

   function Fail return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Fail, 1, EOP));
   end Fail;

   -----------
   -- Fence --
   -----------

   --  Simple case

   function Fence return Pattern is
   begin
      return (AFC with 1, new PE'(PC_Fence, 1, EOP));
   end Fence;

   --  Function case

   --    +---+     +---+     +---+
   --    | E |---->| P |---->| X |---->
   --    +---+     +---+     +---+

   --  The node numbering of the constituent pattern P is not affected.
   --  Where N is the number of nodes in P, the X node is numbered N + 1,
   --  and the E node is N + 2.

   function Fence (P : Pattern) return Pattern is
      Pat : constant PE_Ptr := Copy (P.P);
      E   : constant PE_Ptr := new PE'(PC_R_Enter, 0, EOP);
      X   : constant PE_Ptr := new PE'(PC_Fence_X, 0, EOP);

   begin
      return (AFC with P.Stk + 1, Bracket (E, Pat, X));
   end Fence;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Pattern) is

      procedure Free is new Unchecked_Deallocation (PE, PE_Ptr);
      procedure Free is new Unchecked_Deallocation (String, String_Ptr);

   begin
      --  Nothing to do if already freed

      if Object.P = null then
         return;

      --  Otherwise we must free all elements

      else
         declare
            Refs : Ref_Array (1 .. Object.P.Index);
            --  References to elements in pattern to be finalized

         begin
            Build_Ref_Array (Object.P, Refs);

            for J in Refs'Range loop
               if Refs (J).Pcode = PC_String then
                  Free (Refs (J).Str);
               end if;

               Free (Refs (J));
            end loop;

            Object.P := null;
         end;
      end if;
   end Finalize;

   -----------
   -- Image --
   -----------

   function Image (P : PE_Ptr) return String is
   begin
      return Image (To_Address (P));
   end Image;

   function Image (P : Pattern) return String is
   begin
      return S (Image (P));
   end Image;

   function Image (P : Pattern) return VString is

      Kill_Ampersand : Boolean := False;
      --  Set True to delete next & to be output to Result

      Result : VString := Nul;
      --  The result is accumulated here, using Append

      Refs : Ref_Array (1 .. P.P.Index);
      --  We build a reference array whose N'th element points to the
      --  pattern element whose Index value is N.

      procedure Delete_Ampersand;
      --  Deletes the ampersand at the end of Result

      procedure Image_Seq (E : PE_Ptr; Succ : PE_Ptr; Paren : Boolean);
      --  E refers to a pattern structure whose successor is given by Succ.
      --  This procedure appends to Result a representation of this pattern.
      --  The Paren parameter indicates whether parentheses are required if
      --  the output is more than one element.

      procedure Image_One (E : in out PE_Ptr);
      --  E refers to a pattern structure. This procedure appends to Result
      --  a representation of the single simple or compound pattern structure
      --  at the start of E and updates E to point to its successor.

      ----------------------
      -- Delete_Ampersand --
      ----------------------

      procedure Delete_Ampersand is
         L : Natural := Length (Result);

      begin
         if L > 2 then
            Delete (Result, L - 1, L);
         end if;
      end Delete_Ampersand;

      ---------------
      -- Image_One --
      ---------------

      procedure Image_One (E : in out PE_Ptr) is

         ER : PE_Ptr := E.Pthen;
         --  Successor set as result in E unless reset

      begin
         case E.Pcode is

            when PC_Cancel =>
               Append (Result, "Cancel");

            when PC_Alt => Alt : declare

               Elmts_In_L : constant IndexT := E.Pthen.Index - E.Alt.Index;
               --  Number of elements in left pattern of alternation.

               Lowest_In_L : constant IndexT := E.Index - Elmts_In_L;
               --  Number of lowest index in elements of left pattern

               E1 : PE_Ptr;

            begin
               --  The successor of the alternation node must have a lower
               --  index than any node that is in the left pattern or a
               --  higher index than the alternation node itself.

               while ER /= EOP
                 and then ER.Index >= Lowest_In_L
                 and then ER.Index < E.Index
               loop
                  ER := ER.Pthen;
               end loop;

               Append (Result, '(');

               E1 := E;
               loop
                  Image_Seq (E1.Pthen, ER, False);
                  Append (Result, " or ");
                  E1 := E1.Alt;
                  exit when E1.Pcode /= PC_Alt;
               end loop;

               Image_Seq (E1, ER, False);
               Append (Result, ')');
            end Alt;

            when PC_Any_CS =>
               Append (Result, "Any (" & Image (To_Sequence (E.CS)) & ')');

            when PC_Any_VF =>
               Append (Result, "Any (" & Str_VF (E.VF) & ')');

            when PC_Any_VP =>
               Append (Result, "Any (" & Str_VP (E.VP) & ')');

            when PC_Arb_X =>
               Append (Result, "Arb");

            when PC_Arbno_S =>
               Append (Result, "Arbno (");
               Image_Seq (E.Alt, E, False);
               Append (Result, ')');

            when PC_Arbno_X =>
               Append (Result, "Arbno (");
               Image_Seq (E.Alt.Pthen, Refs (E.Index - 2), False);
               Append (Result, ')');

            when PC_Assign_Imm =>
               Delete_Ampersand;
               Append (Result, "* " & Str_VP (Refs (E.Index - 1).VP));

            when PC_Assign_OnM =>
               Delete_Ampersand;
               Append (Result, "** " & Str_VP (Refs (E.Index - 1).VP));

            when PC_Any_CH =>
               Append (Result, "Any ('" & E.Char & "')");

            when PC_Bal =>
               Append (Result, "Bal");

            when PC_Break_CH =>
               Append (Result, "Break ('" & E.Char & "')");

            when PC_Break_CS =>
               Append (Result, "Break (" & Image (To_Sequence (E.CS)) & ')');

            when PC_Break_VF =>
               Append (Result, "Break (" & Str_VF (E.VF) & ')');

            when PC_Break_VP =>
               Append (Result, "Break (" & Str_VP (E.VP) & ')');

            when PC_BreakX_CH =>
               Append (Result, "BreakX ('" & E.Char & "')");
               ER := ER.Pthen;

            when PC_BreakX_CS =>
               Append (Result, "BreakX (" & Image (To_Sequence (E.CS)) & ')');
               ER := ER.Pthen;

            when PC_BreakX_VF =>
               Append (Result, "BreakX (" & Str_VF (E.VF) & ')');
               ER := ER.Pthen;

            when PC_BreakX_VP =>
               Append (Result, "BreakX (" & Str_VP (E.VP) & ')');
               ER := ER.Pthen;

            when PC_Char =>
               Append (Result, ''' & E.Char & ''');

            when PC_Fail =>
               Append (Result, "Fail");

            when PC_Fence =>
               Append (Result, "Fence");

            when PC_Fence_X =>
               Append (Result, "Fence (");
               Image_Seq (E.Pthen, Refs (E.Index - 1), False);
               Append (Result, ")");
               ER := Refs (E.Index - 1).Pthen;

            when PC_Len_Nat =>
               Append (Result, "Len (" & E.Nat & ')');

            when PC_Len_NF =>
               Append (Result, "Len (" & Str_NF (E.NF) & ')');

            when PC_Len_NP =>
               Append (Result, "Len (" & Str_NP (E.NP) & ')');

            when PC_NotAny_CH =>
               Append (Result, "NotAny ('" & E.Char & "')");

            when PC_NotAny_CS =>
               Append (Result, "NotAny (" & Image (To_Sequence (E.CS)) & ')');

            when PC_NotAny_VF =>
               Append (Result, "NotAny (" & Str_VF (E.VF) & ')');

            when PC_NotAny_VP =>
               Append (Result, "NotAny (" & Str_VP (E.VP) & ')');

            when PC_NSpan_CH =>
               Append (Result, "NSpan ('" & E.Char & "')");

            when PC_NSpan_CS =>
               Append (Result, "NSpan (" & Image (To_Sequence (E.CS)) & ')');

            when PC_NSpan_VF =>
               Append (Result, "NSpan (" & Str_VF (E.VF) & ')');

            when PC_NSpan_VP =>
               Append (Result, "NSpan (" & Str_VP (E.VP) & ')');

            when PC_Null =>
               Append (Result, """""");

            when PC_Pos_Nat =>
               Append (Result, "Pos (" & E.Nat & ')');

            when PC_Pos_NF =>
               Append (Result, "Pos (" & Str_NF (E.NF) & ')');

            when PC_Pos_NP =>
               Append (Result, "Pos (" & Str_NP (E.NP) & ')');

            when PC_R_Enter =>
               Kill_Ampersand := True;

            when PC_Rest =>
               Append (Result, "Rest");

            when PC_Rpat =>
               Append (Result, "(+ " & Str_PP (E.PP) & ')');

            when PC_Pred_Func =>
               Append (Result, "(+ " & Str_BF (E.BF) & ')');

            when PC_RPos_Nat =>
               Append (Result, "RPos (" & E.Nat & ')');

            when PC_RPos_NF =>
               Append (Result, "RPos (" & Str_NF (E.NF) & ')');

            when PC_RPos_NP =>
               Append (Result, "RPos (" & Str_NP (E.NP) & ')');

            when PC_RTab_Nat =>
               Append (Result, "RTab (" & E.Nat & ')');

            when PC_RTab_NF =>
               Append (Result, "RTab (" & Str_NF (E.NF) & ')');

            when PC_RTab_NP =>
               Append (Result, "RTab (" & Str_NP (E.NP) & ')');

            when PC_Setcur =>
               Append (Result, "Setcur (" & Str_NP (E.Var) & ')');

            when PC_Span_CH =>
               Append (Result, "Span ('" & E.Char & "')");

            when PC_Span_CS =>
               Append (Result, "Span (" & Image (To_Sequence (E.CS)) & ')');

            when PC_Span_VF =>
               Append (Result, "Span (" & Str_VF (E.VF) & ')');

            when PC_Span_VP =>
               Append (Result, "Span (" & Str_VP (E.VP) & ')');

            when PC_String =>
               Append (Result, Image (E.Str.all));

            when PC_String_2 =>
               Append (Result, Image (E.Str2));

            when PC_String_3 =>
               Append (Result, Image (E.Str3));

            when PC_String_4 =>
               Append (Result, Image (E.Str4));

            when PC_String_5 =>
               Append (Result, Image (E.Str5));

            when PC_String_6 =>
               Append (Result, Image (E.Str6));

            when PC_String_VF =>
               Append (Result, "(+" &  Str_VF (E.VF) & ')');

            when PC_String_VP =>
               Append (Result, "(+" & Str_VP (E.VP) & ')');

            when PC_Succeed =>
               Append (Result, "Succeed");

            when PC_Tab_Nat =>
               Append (Result, "Tab (" & E.Nat & ')');

            when PC_Tab_NF =>
               Append (Result, "Tab (" & Str_NF (E.NF) & ')');

            when PC_Tab_NP =>
               Append (Result, "Tab (" & Str_NP (E.NP) & ')');

            when PC_Write_Imm =>
               Append (Result, '(');
               Image_Seq (E, Refs (E.Index - 1), True);
               Append (Result, " * " & Str_FP (Refs (E.Index - 1).FP));
               ER := Refs (E.Index - 1).Pthen;

            when PC_Write_OnM =>
               Append (Result, '(');
               Image_Seq (E.Pthen, Refs (E.Index - 1), True);
               Append (Result, " ** " & Str_FP (Refs (E.Index - 1).FP));
               ER := Refs (E.Index - 1).Pthen;

            --  Other pattern codes should not appear as leading elements

            when PC_Arb_Y      |
                 PC_Arbno_Y    |
                 PC_Assign     |
                 PC_BreakX_X   |
                 PC_EOP        |
                 PC_Fence_Y    |
                 PC_R_Remove   |
                 PC_R_Restore  |
                 PC_Unanchored =>
               Append (Result, "???");

         end case;

         E := ER;
      end Image_One;

      ---------------
      -- Image_Seq --
      ---------------

      procedure Image_Seq (E : PE_Ptr; Succ : PE_Ptr; Paren : Boolean) is
         E1   : PE_Ptr  := E;
         Mult : Boolean := False;
         Indx : Natural := Length (Result);

      begin
         --  The image of EOP is "" (the null string)

         if E = EOP then
            Append (Result, """""");

         --  Else generate appropriate concatenation sequence

         else
            loop
               Image_One (E1);
               exit when E1 = Succ;
               exit when E1 = EOP;
               Mult := True;

               if Kill_Ampersand then
                  Kill_Ampersand := False;
               else
                  Append (Result, " & ");
               end if;
            end loop;
         end if;

         if Mult and Paren then
            Insert (Result, Indx + 1, "(");
            Append (Result, ")");
         end if;
      end Image_Seq;

   --  Start of processing for Image

   begin
      Build_Ref_Array (P.P, Refs);
      Image_Seq (P.P, EOP, False);
      return Result;
   end Image;

   -----------
   -- Is_In --
   -----------

   function Is_In (C : Character; Str : String) return Boolean is
   begin
      for J in Str'Range loop
         if Str (J) = C then
            return True;
         end if;
      end loop;

      return False;
   end Is_In;

   ---------
   -- Len --
   ---------

   function Len (Count : Natural) return Pattern is
   begin
      --  Note, the following is not just an optimization, it is needed
      --  to ensure that Arbno (Len (0)) does not generate an infinite
      --  matching loop (since PC_Len_Nat is OK_For_Simple_Arbno).

      if Count = 0 then
         return (AFC with 0, new PE'(PC_Null, 1, EOP));

      else
         return (AFC with 0, new PE'(PC_Len_Nat, 1, EOP, Count));
      end if;
   end Len;

   function Len (Count : Natural_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Len_NF, 1, EOP, Count));
   end Len;

   function Len (Count : access Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Len_NP, 1, EOP, Natural_Ptr (Count)));
   end Len;

   -----------------
   -- Logic_Error --
   -----------------

   procedure Logic_Error is
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Internal logic error in GNAT.Spitbol.Patterns");
   end Logic_Error;

   -----------
   -- Match --
   -----------

   function Match
     (Subject : VString;
      Pat     : Pattern)
      return    Boolean
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      end if;

      return Start /= 0;
   end Match;

   function Match
     (Subject : String;
      Pat     : Pattern)
      return    Boolean
   is
      Start, Stop : Natural;
      subtype String1 is String (1 .. Subject'Length);

   begin
      if Debug_Mode then
         XMatchD (String1 (Subject), Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (String1 (Subject), Pat.P, Pat.Stk, Start, Stop);
      end if;

      return Start /= 0;
   end Match;

   function Match
     (Subject : VString_Var;
      Pat     : Pattern;
      Replace : VString)
      return    Boolean
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      end if;

      if Start = 0 then
         return False;
      else
         Replace_Slice
           (Subject'Unrestricted_Access.all,
            Start, Stop, Get_String (Replace).all);
         return True;
      end if;
   end Match;

   function Match
     (Subject : VString_Var;
      Pat     : Pattern;
      Replace : String)
      return    Boolean
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      end if;

      if Start = 0 then
         return False;
      else
         Replace_Slice
           (Subject'Unrestricted_Access.all, Start, Stop, Replace);
         return True;
      end if;
   end Match;

   procedure Match
     (Subject : VString;
      Pat     : Pattern)
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      end if;

   end Match;

   procedure Match
     (Subject : String;
      Pat     : Pattern)
   is
      Start, Stop : Natural;
      subtype String1 is String (1 .. Subject'Length);
   begin
      if Debug_Mode then
         XMatchD (String1 (Subject), Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (String1 (Subject), Pat.P, Pat.Stk, Start, Stop);
      end if;
   end Match;

   procedure Match
     (Subject : in out VString;
      Pat     : Pattern;
      Replace : VString)
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      end if;

      if Start /= 0 then
         Replace_Slice (Subject, Start, Stop, Get_String (Replace).all);
      end if;
   end Match;

   procedure Match
     (Subject : in out VString;
      Pat     : Pattern;
      Replace : String)
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      end if;

      if Start /= 0 then
         Replace_Slice (Subject, Start, Stop, Replace);
      end if;
   end Match;

   function Match
     (Subject : VString;
      Pat     : PString)
      return    Boolean
   is
      Pat_Len : constant Natural       := Pat'Length;
      Sub_Len : constant Natural       := Length (Subject);
      Sub_Str : constant String_Access := Get_String (Subject);

   begin
      if Anchored_Mode then
         if Pat_Len > Sub_Len then
            return False;
         else
            return Pat = Sub_Str.all (1 .. Pat_Len);
         end if;

      else
         for J in 1 .. Sub_Len - Pat_Len + 1 loop
            if Pat = Sub_Str.all (J .. J + (Pat_Len - 1)) then
               return True;
            end if;
         end loop;

         return False;
      end if;
   end Match;

   function Match
     (Subject : String;
      Pat     : PString)
      return    Boolean
   is
      Pat_Len : constant Natural := Pat'Length;
      Sub_Len : constant Natural := Subject'Length;
      SFirst  : constant Natural := Subject'First;

   begin
      if Anchored_Mode then
         if Pat_Len > Sub_Len then
            return False;
         else
            return Pat = Subject (SFirst .. SFirst + Pat_Len - 1);
         end if;

      else
         for J in SFirst .. SFirst + Sub_Len - Pat_Len loop
            if Pat = Subject (J .. J + (Pat_Len - 1)) then
               return True;
            end if;
         end loop;

         return False;
      end if;
   end Match;

   function Match
     (Subject : VString_Var;
      Pat     : PString;
      Replace : VString)
      return    Boolean
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      end if;

      if Start = 0 then
         return False;
      else
         Replace_Slice
           (Subject'Unrestricted_Access.all,
            Start, Stop, Get_String (Replace).all);
         return True;
      end if;
   end Match;

   function Match
     (Subject : VString_Var;
      Pat     : PString;
      Replace : String)
      return    Boolean
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      end if;

      if Start = 0 then
         return False;
      else
         Replace_Slice
           (Subject'Unrestricted_Access.all, Start, Stop, Replace);
         return True;
      end if;
   end Match;

   procedure Match
     (Subject : VString;
      Pat     : PString)
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      end if;
   end Match;

   procedure Match
     (Subject : String;
      Pat     : PString)
   is
      Start, Stop : Natural;
      subtype String1 is String (1 .. Subject'Length);

   begin
      if Debug_Mode then
         XMatchD (String1 (Subject), S_To_PE (Pat), 0, Start, Stop);
      else
         XMatch  (String1 (Subject), S_To_PE (Pat), 0, Start, Stop);
      end if;
   end Match;

   procedure Match
     (Subject : in out VString;
      Pat     : PString;
      Replace : VString)
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      end if;

      if Start /= 0 then
         Replace_Slice (Subject, Start, Stop, Get_String (Replace).all);
      end if;
   end Match;

   procedure Match
     (Subject : in out VString;
      Pat     : PString;
      Replace : String)
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, S_To_PE (Pat), 0, Start, Stop);
      end if;

      if Start /= 0 then
         Replace_Slice (Subject, Start, Stop, Replace);
      end if;
   end Match;

   function Match
     (Subject : VString_Var;
      Pat     : Pattern;
      Result  : Match_Result_Var)
      return    Boolean
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      end if;

      if Start = 0 then
         Result'Unrestricted_Access.all.Var := null;
         return False;

      else
         Result'Unrestricted_Access.all.Var   := Subject'Unrestricted_Access;
         Result'Unrestricted_Access.all.Start := Start;
         Result'Unrestricted_Access.all.Stop  := Stop;
         return True;
      end if;
   end Match;

   procedure Match
     (Subject : in out VString;
      Pat     : Pattern;
      Result  : out Match_Result)
   is
      Start, Stop : Natural;

   begin
      if Debug_Mode then
         XMatchD (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      else
         XMatch  (Get_String (Subject).all, Pat.P, Pat.Stk, Start, Stop);
      end if;

      if Start = 0 then
         Result.Var := null;

      else
         Result.Var   := Subject'Unrestricted_Access;
         Result.Start := Start;
         Result.Stop  := Stop;
      end if;
   end Match;

   ---------------
   -- New_LineD --
   ---------------

   procedure New_LineD is
   begin
      if Internal_Debug then
         New_Line;
      end if;
   end New_LineD;

   ------------
   -- NotAny --
   ------------

   function NotAny (Str : String) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NotAny_CS, 1, EOP, To_Set (Str)));
   end NotAny;

   function NotAny (Str : VString) return Pattern is
   begin
      return NotAny (S (Str));
   end NotAny;

   function NotAny (Str : Character) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NotAny_CH, 1, EOP, Str));
   end NotAny;

   function NotAny (Str : Character_Set) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NotAny_CS, 1, EOP, Str));
   end NotAny;

   function NotAny (Str : access VString) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NotAny_VP, 1, EOP, VString_Ptr (Str)));
   end NotAny;

   function NotAny (Str : VString_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NotAny_VF, 1, EOP, Str));
   end NotAny;

   -----------
   -- NSpan --
   -----------

   function NSpan (Str : String) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NSpan_CS, 1, EOP, To_Set (Str)));
   end NSpan;

   function NSpan (Str : VString) return Pattern is
   begin
      return NSpan (S (Str));
   end NSpan;

   function NSpan (Str : Character) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NSpan_CH, 1, EOP, Str));
   end NSpan;

   function NSpan (Str : Character_Set) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NSpan_CS, 1, EOP, Str));
   end NSpan;

   function NSpan (Str : access VString) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NSpan_VP, 1, EOP, VString_Ptr (Str)));
   end NSpan;

   function NSpan (Str : VString_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_NSpan_VF, 1, EOP, Str));
   end NSpan;

   ---------
   -- Pos --
   ---------

   function Pos (Count : Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Pos_Nat, 1, EOP, Count));
   end Pos;

   function Pos (Count : Natural_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Pos_NF, 1, EOP, Count));
   end Pos;

   function Pos (Count : access Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Pos_NP, 1, EOP, Natural_Ptr (Count)));
   end Pos;

   ----------
   -- PutD --
   ----------

   procedure PutD (Str : String) is
   begin
      if Internal_Debug then
         Put (Str);
      end if;
   end PutD;

   ---------------
   -- Put_LineD --
   ---------------

   procedure Put_LineD (Str : String) is
   begin
      if Internal_Debug then
         Put_Line (Str);
      end if;
   end Put_LineD;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Result  : in out Match_Result;
      Replace : VString)
   is
   begin
      if Result.Var /= null then
         Replace_Slice
           (Result.Var.all,
            Result.Start,
            Result.Stop,
            Get_String (Replace).all);
         Result.Var := null;
      end if;
   end Replace;

   ----------
   -- Rest --
   ----------

   function Rest return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Rest, 1, EOP));
   end Rest;

   ----------
   -- Rpos --
   ----------

   function Rpos (Count : Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_RPos_Nat, 1, EOP, Count));
   end Rpos;

   function Rpos (Count : Natural_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_RPos_NF, 1, EOP, Count));
   end Rpos;

   function Rpos (Count : access Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_RPos_NP, 1, EOP, Natural_Ptr (Count)));
   end Rpos;

   ----------
   -- Rtab --
   ----------

   function Rtab (Count : Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_RTab_Nat, 1, EOP, Count));
   end Rtab;

   function Rtab (Count : Natural_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_RTab_NF, 1, EOP, Count));
   end Rtab;

   function Rtab (Count : access Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_RTab_NP, 1, EOP, Natural_Ptr (Count)));
   end Rtab;

   -------------
   -- S_To_PE --
   -------------

   function S_To_PE (Str : PString) return PE_Ptr is
      Len : constant Natural := Str'Length;

   begin
      case Len is
         when 0 =>
            return new PE'(PC_Null,     1, EOP);

         when 1 =>
            return new PE'(PC_Char,     1, EOP, Str (1));

         when 2 =>
            return new PE'(PC_String_2, 1, EOP, Str);

         when 3 =>
            return new PE'(PC_String_3, 1, EOP, Str);

         when 4 =>
            return new PE'(PC_String_4, 1, EOP, Str);

         when 5 =>
            return new PE'(PC_String_5, 1, EOP, Str);

         when 6 =>
            return new PE'(PC_String_6, 1, EOP, Str);

         when others =>
            return new PE'(PC_String, 1, EOP, new String'(Str));

      end case;
   end S_To_PE;

   -------------------
   -- Set_Successor --
   -------------------

   --  Note: this procedure is not used by the normal concatenation circuit,
   --  since other fixups are required on the left operand in this case, and
   --  they might as well be done all together.

   procedure Set_Successor (Pat : PE_Ptr; Succ : PE_Ptr) is
   begin
      if Pat = null then
         Uninitialized_Pattern;

      elsif Pat = EOP then
         Logic_Error;

      else
         declare
            Refs : Ref_Array (1 .. Pat.Index);
            --  We build a reference array for L whose N'th element points to
            --  the pattern element of L whose original Index value is N.

            P : PE_Ptr;

         begin
            Build_Ref_Array (Pat, Refs);

            for J in Refs'Range loop
               P := Refs (J);

               if P.Pthen = EOP then
                  P.Pthen := Succ;
               end if;

               if P.Pcode in PC_Has_Alt and then P.Alt = EOP then
                  P.Alt := Succ;
               end if;
            end loop;
         end;
      end if;
   end Set_Successor;

   ------------
   -- Setcur --
   ------------

   function Setcur (Var : access Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Setcur, 1, EOP, Natural_Ptr (Var)));
   end Setcur;

   ----------
   -- Span --
   ----------

   function Span (Str : String) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Span_CS, 1, EOP, To_Set (Str)));
   end Span;

   function Span (Str : VString) return Pattern is
   begin
      return Span (S (Str));
   end Span;

   function Span (Str : Character) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Span_CH, 1, EOP, Str));
   end Span;

   function Span (Str : Character_Set) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Span_CS, 1, EOP, Str));
   end Span;

   function Span (Str : access VString) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Span_VP, 1, EOP, VString_Ptr (Str)));
   end Span;

   function Span (Str : VString_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Span_VF, 1, EOP, Str));
   end Span;

   ------------
   -- Str_BF --
   ------------

   function Str_BF (A : Boolean_Func) return String is
      function To_A is new Unchecked_Conversion (Boolean_Func, Address);

   begin
      return "BF(" & Image (To_A (A)) & ')';
   end Str_BF;

   ------------
   -- Str_FP --
   ------------

   function Str_FP (A : File_Ptr) return String is
   begin
      return "FP(" & Image (A.all'Address) & ')';
   end Str_FP;

   ------------
   -- Str_NF --
   ------------

   function Str_NF (A : Natural_Func) return String is
      function To_A is new Unchecked_Conversion (Natural_Func, Address);

   begin
      return "NF(" & Image (To_A (A)) & ')';
   end Str_NF;

   ------------
   -- Str_NP --
   ------------

   function Str_NP (A : Natural_Ptr) return String is
   begin
      return "NP(" & Image (A.all'Address) & ')';
   end Str_NP;

   ------------
   -- Str_PP --
   ------------

   function Str_PP (A : Pattern_Ptr) return String is
   begin
      return "PP(" & Image (A.all'Address) & ')';
   end Str_PP;

   ------------
   -- Str_VF --
   ------------

   function Str_VF (A : VString_Func) return String is
      function To_A is new Unchecked_Conversion (VString_Func, Address);

   begin
      return "VF(" & Image (To_A (A)) & ')';
   end Str_VF;

   ------------
   -- Str_VP --
   ------------

   function Str_VP (A : VString_Ptr) return String is
   begin
      return "VP(" & Image (A.all'Address) & ')';
   end Str_VP;

   -------------
   -- Succeed --
   -------------

   function Succeed return Pattern is
   begin
      return (AFC with 1, new PE'(PC_Succeed, 1, EOP));
   end Succeed;

   ---------
   -- Tab --
   ---------

   function Tab (Count : Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Tab_Nat, 1, EOP, Count));
   end Tab;

   function Tab (Count : Natural_Func) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Tab_NF, 1, EOP, Count));
   end Tab;

   function Tab (Count : access Natural) return Pattern is
   begin
      return (AFC with 0, new PE'(PC_Tab_NP, 1, EOP, Natural_Ptr (Count)));
   end Tab;

   ---------------------------
   -- Uninitialized_Pattern --
   ---------------------------

   procedure Uninitialized_Pattern is
   begin
      Raise_Exception
        (Program_Error'Identity,
         "uninitialized value of type GNAT.Spitbol.Patterns.Pattern");
   end Uninitialized_Pattern;

   ------------
   -- XMatch --
   ------------

   procedure XMatch
     (Subject : String;
      Pat_P   : PE_Ptr;
      Pat_S   : Natural;
      Start   : out Natural;
      Stop    : out Natural)
   is
      Node : PE_Ptr;
      --  Pointer to current pattern node. Initialized from Pat_P, and then
      --  updated as the match proceeds through its constituent elements.

      Length : constant Natural := Subject'Length;
      --  Length of string (= Subject'Last, since Subject'First is always 1)

      Cursor : Integer := 0;
      --  If the value is non-negative, then this value is the index showing
      --  the current position of the match in the subject string. The next
      --  character to be matched is at Subject (Cursor + 1). Note that since
      --  our view of the subject string in XMatch always has a lower bound
      --  of one, regardless of original bounds, that this definition exactly
      --  corresponds to the cursor value as referenced by functions like Pos.
      --
      --  If the value is negative, then this is a saved stack pointer,
      --  typically a base pointer of an inner or outer region. Cursor
      --  temporarily holds such a value when it is popped from the stack
      --  by Fail. In all cases, Cursor is reset to a proper non-negative
      --  cursor value before the match proceeds (e.g. by propagating the
      --  failure and popping a "real" cursor value from the stack.

      PE_Unanchored : aliased PE := (PC_Unanchored, 0, Pat_P);
      --  Dummy pattern element used in the unanchored case.

      Stack : Stack_Type;
      --  The pattern matching failure stack for this call to Match

      Stack_Ptr : Stack_Range;
      --  Current stack pointer. This points to the top element of the stack
      --  that is currently in use. At the outer level this is the special
      --  entry placed on the stack according to the anchor mode.

      Stack_Init : constant Stack_Range := Stack'First + 1;
      --  This is the initial value of the Stack_Ptr and Stack_Base. The
      --  initial (Stack'First) element of the stack is not used so that
      --  when we pop the last element off, Stack_Ptr is still in range.

      Stack_Base : Stack_Range;
      --  This value is the stack base value, i.e. the stack pointer for the
      --  first history stack entry in the current stack region. See separate
      --  section on handling of recursive pattern matches.

      Assign_OnM : Boolean := False;
      --  Set True if assign-on-match or write-on-match operations may be
      --  present in the history stack, which must then be scanned on a
      --  successful match.

      procedure Pop_Region;
      pragma Inline (Pop_Region);
      --  Used at the end of processing of an inner region. if the inner
      --  region left no stack entries, then all trace of it is removed.
      --  Otherwise a PC_Restore_Region entry is pushed to ensure proper
      --  handling of alternatives in the inner region.

      procedure Push (Node : PE_Ptr);
      pragma Inline (Push);
      --  Make entry in pattern matching stack with current cursor valeu

      procedure Push_Region;
      pragma Inline (Push_Region);
      --  This procedure makes a new region on the history stack. The
      --  caller first establishes the special entry on the stack, but
      --  does not push the stack pointer. Then this call stacks a
      --  PC_Remove_Region node, on top of this entry, using the cursor
      --  field of the PC_Remove_Region entry to save the outer level
      --  stack base value, and resets the stack base to point to this
      --  PC_Remove_Region node.

      ----------------
      -- Pop_Region --
      ----------------

      procedure Pop_Region is
      begin
         --  If nothing was pushed in the inner region, we can just get
         --  rid of it entirely, leaving no traces that it was ever there

         if Stack_Ptr = Stack_Base then
            Stack_Ptr := Stack_Base - 2;
            Stack_Base := Stack (Stack_Ptr + 2).Cursor;

         --  If stuff was pushed in the inner region, then we have to
         --  push a PC_R_Restore node so that we properly handle possible
         --  rematches within the region.

         else
            Stack_Ptr := Stack_Ptr + 1;
            Stack (Stack_Ptr).Cursor := Stack_Base;
            Stack (Stack_Ptr).Node   := CP_R_Restore'Access;
            Stack_Base := Stack (Stack_Base).Cursor;
         end if;
      end Pop_Region;

      ----------
      -- Push --
      ----------

      procedure Push (Node : PE_Ptr) is
      begin
         Stack_Ptr := Stack_Ptr + 1;
         Stack (Stack_Ptr).Cursor := Cursor;
         Stack (Stack_Ptr).Node   := Node;
      end Push;

      -----------------
      -- Push_Region --
      -----------------

      procedure Push_Region is
      begin
         Stack_Ptr := Stack_Ptr + 2;
         Stack (Stack_Ptr).Cursor := Stack_Base;
         Stack (Stack_Ptr).Node   := CP_R_Remove'Access;
         Stack_Base := Stack_Ptr;
      end Push_Region;

   --  Start of processing for XMatch

   begin
      if Pat_P = null then
         Uninitialized_Pattern;
      end if;

      --  Check we have enough stack for this pattern. This check deals with
      --  every possibility except a match of a recursive pattern, where we
      --  make a check at each recursion level.

      if Pat_S >= Stack_Size - 1 then
         raise Pattern_Stack_Overflow;
      end if;

      --  In anchored mode, the bottom entry on the stack is an abort entry

      if Anchored_Mode then
         Stack (Stack_Init).Node   := CP_Cancel'Access;
         Stack (Stack_Init).Cursor := 0;

      --  In unanchored more, the bottom entry on the stack references
      --  the special pattern element PE_Unanchored, whose Pthen field
      --  points to the initial pattern element. The cursor value in this
      --  entry is the number of anchor moves so far.

      else
         Stack (Stack_Init).Node   := PE_Unanchored'Unchecked_Access;
         Stack (Stack_Init).Cursor := 0;
      end if;

      Stack_Ptr    := Stack_Init;
      Stack_Base   := Stack_Ptr;
      Cursor       := 0;
      Node         := Pat_P;
      goto Match;

      -----------------------------------------
      -- Main Pattern Matching State Control --
      -----------------------------------------

      --  This is a state machine which uses gotos to change state. The
      --  initial state is Match, to initiate the matching of the first
      --  element, so the goto Match above starts the match. In the
      --  following descriptions, we indicate the global values that
      --  are relevant for the state transition.

      --  Come here if entire match fails

      <<Match_Fail>>
         Start := 0;
         Stop  := 0;
         return;

      --  Come here if entire match succeeds

      --    Cursor        current position in subject string

      <<Match_Succeed>>
         Start := Stack (Stack_Init).Cursor + 1;
         Stop  := Cursor;

         --  Scan history stack for deferred assignments or writes

         if Assign_OnM then
            for S in Stack_Init .. Stack_Ptr loop
               if Stack (S).Node = CP_Assign'Access then
                  declare
                     Inner_Base    : constant Stack_Range :=
                                       Stack (S + 1).Cursor;
                     Special_Entry : constant Stack_Range :=
                                       Inner_Base - 1;
                     Node_OnM      : constant PE_Ptr  :=
                                       Stack (Special_Entry).Node;
                     Start         : constant Natural :=
                                       Stack (Special_Entry).Cursor + 1;
                     Stop          : constant Natural := Stack (S).Cursor;

                  begin
                     if Node_OnM.Pcode = PC_Assign_OnM then
                        Set_String (Node_OnM.VP.all, Subject (Start .. Stop));

                     elsif Node_OnM.Pcode = PC_Write_OnM then
                        Put_Line (Node_OnM.FP.all, Subject (Start .. Stop));

                     else
                        Logic_Error;
                     end if;
                  end;
               end if;
            end loop;
         end if;

         return;

      --  Come here if attempt to match current element fails

      --    Stack_Base    current stack base
      --    Stack_Ptr     current stack pointer

      <<Fail>>
         Cursor := Stack (Stack_Ptr).Cursor;
         Node   := Stack (Stack_Ptr).Node;
         Stack_Ptr := Stack_Ptr - 1;
         goto Match;

      --  Come here if attempt to match current element succeeds

      --    Cursor        current position in subject string
      --    Node          pointer to node successfully matched
      --    Stack_Base    current stack base
      --    Stack_Ptr     current stack pointer

      <<Succeed>>
         Node := Node.Pthen;

      --  Come here to match the next pattern element

      --    Cursor        current position in subject string
      --    Node          pointer to node to be matched
      --    Stack_Base    current stack base
      --    Stack_Ptr     current stack pointer

      <<Match>>

      --------------------------------------------------
      -- Main Pattern Match Element Matching Routines --
      --------------------------------------------------

      --  Here is the case statement that processes the current node. The
      --  processing for each element does one of five things:

      --    goto Succeed        to move to the successor
      --    goto Match_Succeed  if the entire match succeeds
      --    goto Match_Fail     if the entire match fails
      --    goto Fail           to signal failure of current match

      --  Processing is NOT allowed to fall through

      case Node.Pcode is

         --  Cancel

         when PC_Cancel =>
            goto Match_Fail;

         --  Alternation

         when PC_Alt =>
            Push (Node.Alt);
            Node := Node.Pthen;
            goto Match;

         --  Any (one character case)

         when PC_Any_CH =>
            if Cursor < Length
              and then Subject (Cursor + 1) = Node.Char
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Any (character set case)

         when PC_Any_CS =>
            if Cursor < Length
              and then Is_In (Subject (Cursor + 1), Node.CS)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Any (string function case)

         when PC_Any_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            if Cursor < Length
              and then Is_In (Subject (Cursor + 1), Str.all)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Any (string pointer case)

         when PC_Any_VP => declare
            Str : constant String_Access := Get_String (Node.VP.all);

         begin
            if Cursor < Length
              and then Is_In (Subject (Cursor + 1), Str.all)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Arb (initial match)

         when PC_Arb_X =>
            Push (Node.Alt);
            Node := Node.Pthen;
            goto Match;

         --  Arb (extension)

         when PC_Arb_Y  =>
            if Cursor < Length then
               Cursor := Cursor + 1;
               Push (Node);
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Arbno_S (simple Arbno initialize). This is the node that
         --  initiates the match of a simple Arbno structure.

         when PC_Arbno_S =>
            Push (Node.Alt);
            Node := Node.Pthen;
            goto Match;

         --  Arbno_X (Arbno initialize). This is the node that initiates
         --  the match of a complex Arbno structure.

         when PC_Arbno_X =>
            Push (Node.Alt);
            Node := Node.Pthen;
            goto Match;

         --  Arbno_Y (Arbno rematch). This is the node that is executed
         --  following successful matching of one instance of a complex
         --  Arbno pattern.

         when PC_Arbno_Y => declare
            Null_Match : Boolean := (Cursor = Stack (Stack_Base - 1).Cursor);

         begin
            Pop_Region;

            --  If arbno extension matched null, then immediately fail

            if Null_Match then
               goto Fail;
            end if;

            --  Here we must do a stack check to make sure enough stack
            --  is left. This check will happen once for each instance of
            --  the Arbno pattern that is matched. The Nat field of a
            --  PC_Arbno pattern contains the maximum stack entries needed
            --  for the Arbno with one instance and the successor pattern

            if Stack_Ptr + Node.Nat >= Stack'Last then
               raise Pattern_Stack_Overflow;
            end if;

            goto Succeed;
         end;

         --  Assign. If this node is executed, it means the assign-on-match
         --  or write-on-match operation will not happen after all, so we
         --  is propagate the failure, removing the PC_Assign node.

         when PC_Assign =>
            goto Fail;

         --  Assign immediate. This node performs the actual assignment.

         when PC_Assign_Imm =>
            Set_String
              (Node.VP.all,
               Subject (Stack (Stack_Base - 1).Cursor + 1 .. Cursor));
            Pop_Region;
            goto Succeed;

         --  Assign on match. This node sets up for the eventual assignment

         when PC_Assign_OnM =>
            Stack (Stack_Base - 1).Node := Node;
            Push (CP_Assign'Access);
            Pop_Region;
            Assign_OnM := True;
            goto Succeed;

         --  Bal

         when PC_Bal =>
            if Cursor >= Length or else Subject (Cursor + 1) = ')' then
               goto Fail;

            elsif Subject (Cursor + 1) = '(' then
               declare
                  Paren_Count : Natural := 1;

               begin
                  loop
                     Cursor := Cursor + 1;

                     if Cursor >= Length then
                        goto Fail;

                     elsif Subject (Cursor + 1) = '(' then
                        Paren_Count := Paren_Count + 1;

                     elsif Subject (Cursor + 1) = ')' then
                        Paren_Count := Paren_Count - 1;
                        exit when Paren_Count = 0;
                     end if;
                  end loop;
               end;
            end if;

            Cursor := Cursor + 1;
            Push (Node);
            goto Succeed;

         --  Break (one character case)

         when PC_Break_CH =>
            while Cursor < Length loop
               if Subject (Cursor + 1) = Node.Char then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;

         --  Break (character set case)

         when PC_Break_CS =>
            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Node.CS) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;

         --  Break (string function case)

         when PC_Break_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Str.all) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;
         end;

         --  Break (string pointer case)

         when PC_Break_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Str.all) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;
         end;

         --  BreakX (one character case)

         when PC_BreakX_CH =>
            while Cursor < Length loop
               if Subject (Cursor + 1) = Node.Char then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;

         --  BreakX (character set case)

         when PC_BreakX_CS =>
            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Node.CS) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;

         --  BreakX (string function case)

         when PC_BreakX_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Str.all) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;
         end;

         --  BreakX (string pointer case)

         when PC_BreakX_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Str.all) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;
         end;

         --  BreakX_X (BreakX extension). See section on "Compound Pattern
         --  Structures". This node is the alternative that is stacked to
         --  skip past the break character and extend the break.

         when PC_BreakX_X =>
            Cursor := Cursor + 1;
            goto Succeed;

         --  Character (one character string)

         when PC_Char =>
            if Cursor < Length
              and then Subject (Cursor + 1) = Node.Char
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  End of Pattern

         when PC_EOP =>
            if Stack_Base = Stack_Init then
               goto Match_Succeed;

            --  End of recursive inner match. See separate section on
            --  handing of recursive pattern matches for details.

            else
               Node := Stack (Stack_Base - 1).Node;
               Pop_Region;
               goto Match;
            end if;

         --  Fail

         when PC_Fail =>
            goto Fail;

         --  Fence (built in pattern)

         when PC_Fence =>
            Push (CP_Cancel'Access);
            goto Succeed;

         --  Fence function node X. This is the node that gets control
         --  after a successful match of the fenced pattern.

         when PC_Fence_X =>
            Stack_Ptr := Stack_Ptr + 1;
            Stack (Stack_Ptr).Cursor := Stack_Base;
            Stack (Stack_Ptr).Node   := CP_Fence_Y'Access;
            Stack_Base := Stack (Stack_Base).Cursor;
            goto Succeed;

         --  Fence function node Y. This is the node that gets control on
         --  a failure that occurs after the fenced pattern has matched.

         --  Note: the Cursor at this stage is actually the inner stack
         --  base value. We don't reset this, but we do use it to strip
         --  off all the entries made by the fenced pattern.

         when PC_Fence_Y =>
            Stack_Ptr := Cursor - 2;
            goto Fail;

         --  Len (integer case)

         when PC_Len_Nat =>
            if Cursor + Node.Nat > Length then
               goto Fail;
            else
               Cursor := Cursor + Node.Nat;
               goto Succeed;
            end if;

         --  Len (Integer function case)

         when PC_Len_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            if Cursor + N > Length then
               goto Fail;
            else
               Cursor := Cursor + N;
               goto Succeed;
            end if;
         end;

         --  Len (integer pointer case)

         when PC_Len_NP =>
            if Cursor + Node.NP.all > Length then
               goto Fail;
            else
               Cursor := Cursor + Node.NP.all;
               goto Succeed;
            end if;

         --  NotAny (one character case)

         when PC_NotAny_CH =>
            if Cursor < Length
              and then Subject (Cursor + 1) /= Node.Char
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  NotAny (character set case)

         when PC_NotAny_CS =>
            if Cursor < Length
              and then not Is_In (Subject (Cursor + 1), Node.CS)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  NotAny (string function case)

         when PC_NotAny_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            if Cursor < Length
              and then
                not Is_In (Subject (Cursor + 1), Str.all)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  NotAny (string pointer case)

         when PC_NotAny_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            if Cursor < Length
              and then
                not Is_In (Subject (Cursor + 1), Str.all)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  NSpan (one character case)

         when PC_NSpan_CH =>
            while Cursor < Length
              and then Subject (Cursor + 1) = Node.Char
            loop
               Cursor := Cursor + 1;
            end loop;

            goto Succeed;

         --  NSpan (character set case)

         when PC_NSpan_CS =>
            while Cursor < Length
              and then Is_In (Subject (Cursor + 1), Node.CS)
            loop
               Cursor := Cursor + 1;
            end loop;

            goto Succeed;

         --  NSpan (string function case)

         when PC_NSpan_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            while Cursor < Length
              and then Is_In (Subject (Cursor + 1), Str.all)
            loop
               Cursor := Cursor + 1;
            end loop;

            goto Succeed;
         end;

         --  NSpan (string pointer case)

         when PC_NSpan_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            while Cursor < Length
              and then Is_In (Subject (Cursor + 1), Str.all)
            loop
               Cursor := Cursor + 1;
            end loop;

            goto Succeed;
         end;

         --  Null string

         when PC_Null =>
            goto Succeed;

         --  Pos (integer case)

         when PC_Pos_Nat =>
            if Cursor = Node.Nat then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Pos (Integer function case)

         when PC_Pos_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            if Cursor = N then
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Pos (integer pointer case)

         when PC_Pos_NP =>
            if Cursor = Node.NP.all then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Predicate function

         when PC_Pred_Func =>
            if Node.BF.all then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Region Enter. Initiate new pattern history stack region

         when PC_R_Enter =>
            Stack (Stack_Ptr + 1).Cursor := Cursor;
            Push_Region;
            goto Succeed;

         --  Region Remove node. This is the node stacked by an R_Enter.
         --  It removes the special format stack entry right underneath, and
         --  then restores the outer level stack base and signals failure.

         --  Note: the cursor value at this stage is actually the (negative)
         --  stack base value for the outer level.

         when PC_R_Remove =>
            Stack_Base := Cursor;
            Stack_Ptr := Stack_Ptr - 1;
            goto Fail;

         --  Region restore node. This is the node stacked at the end of an
         --  inner level match. Its function is to restore the inner level
         --  region, so that alternatives in this region can be sought.

         --  Note: the Cursor at this stage is actually the negative of the
         --  inner stack base value, which we use to restore the inner region.

         when PC_R_Restore =>
            Stack_Base := Cursor;
            goto Fail;

         --  Rest

         when PC_Rest =>
            Cursor := Length;
            goto Succeed;

         --  Initiate recursive match (pattern pointer case)

         when PC_Rpat =>
            Stack (Stack_Ptr + 1).Node := Node.Pthen;
            Push_Region;

            if Stack_Ptr + Node.PP.all.Stk >= Stack_Size then
               raise Pattern_Stack_Overflow;
            else
               Node := Node.PP.all.P;
               goto Match;
            end if;

         --  RPos (integer case)

         when PC_RPos_Nat =>
            if Cursor = (Length - Node.Nat) then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  RPos (integer function case)

         when PC_RPos_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            if Length - Cursor = N then
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  RPos (integer pointer case)

         when PC_RPos_NP =>
            if Cursor = (Length - Node.NP.all) then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  RTab (integer case)

         when PC_RTab_Nat =>
            if Cursor <= (Length - Node.Nat) then
               Cursor := Length - Node.Nat;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  RTab (integer function case)

         when PC_RTab_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            if Length - Cursor >= N then
               Cursor := Length - N;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  RTab (integer pointer case)

         when PC_RTab_NP =>
            if Cursor <= (Length - Node.NP.all) then
               Cursor := Length - Node.NP.all;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Cursor assignment

         when PC_Setcur =>
            Node.Var.all := Cursor;
            goto Succeed;

         --  Span (one character case)

         when PC_Span_CH => declare
            P : Natural := Cursor;

         begin
            while P < Length
              and then Subject (P + 1) = Node.Char
            loop
               P := P + 1;
            end loop;

            if P /= Cursor then
               Cursor := P;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Span (character set case)

         when PC_Span_CS => declare
            P : Natural := Cursor;

         begin
            while P < Length
              and then Is_In (Subject (P + 1), Node.CS)
            loop
               P := P + 1;
            end loop;

            if P /= Cursor then
               Cursor := P;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Span (string function case)

         when PC_Span_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);
            P   : Natural := Cursor;

         begin
            while P < Length
              and then Is_In (Subject (P + 1), Str.all)
            loop
               P := P + 1;
            end loop;

            if P /= Cursor then
               Cursor := P;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Span (string pointer case)

         when PC_Span_VP => declare
            Str : String_Access := Get_String (Node.VP.all);
            P   : Natural := Cursor;

         begin
            while P < Length
              and then Is_In (Subject (P + 1), Str.all)
            loop
               P := P + 1;
            end loop;

            if P /= Cursor then
               Cursor := P;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  String (two character case)

         when PC_String_2 =>
            if (Length - Cursor) >= 2
              and then Subject (Cursor + 1 .. Cursor + 2) = Node.Str2
            then
               Cursor := Cursor + 2;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (three character case)

         when PC_String_3 =>
            if (Length - Cursor) >= 3
              and then Subject (Cursor + 1 .. Cursor + 3) = Node.Str3
            then
               Cursor := Cursor + 3;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (four character case)

         when PC_String_4 =>
            if (Length - Cursor) >= 4
              and then Subject (Cursor + 1 .. Cursor + 4) = Node.Str4
            then
               Cursor := Cursor + 4;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (five character case)

         when PC_String_5 =>
            if (Length - Cursor) >= 5
              and then Subject (Cursor + 1 .. Cursor + 5) = Node.Str5
            then
               Cursor := Cursor + 5;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (six character case)

         when PC_String_6 =>
            if (Length - Cursor) >= 6
              and then Subject (Cursor + 1 .. Cursor + 6) = Node.Str6
            then
               Cursor := Cursor + 6;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (case of more than six characters)

         when PC_String => declare
            Len : constant Natural := Node.Str'Length;

         begin
            if (Length - Cursor) >= Len
              and then Node.Str.all = Subject (Cursor + 1 .. Cursor + Len)
            then
               Cursor := Cursor + Len;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  String (function case)

         when PC_String_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);
            Len : constant Natural       := Str'Length;

         begin
            if (Length - Cursor) >= Len
              and then Str.all = Subject (Cursor + 1 .. Cursor + Len)
            then
               Cursor := Cursor + Len;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  String (pointer case)

         when PC_String_VP => declare
            S   : String_Access := Get_String (Node.VP.all);
            Len : constant Natural := S'Length;

         begin
            if (Length - Cursor) >= Len
              and then S.all = Subject (Cursor + 1 .. Cursor + Len)
            then
               Cursor := Cursor + Len;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Succeed

         when PC_Succeed =>
            Push (Node);
            goto Succeed;

         --  Tab (integer case)

         when PC_Tab_Nat =>
            if Cursor <= Node.Nat then
               Cursor := Node.Nat;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Tab (integer function case)

         when PC_Tab_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            if Cursor <= N then
               Cursor := N;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Tab (integer pointer case)

         when PC_Tab_NP =>
            if Cursor <= Node.NP.all then
               Cursor := Node.NP.all;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Unanchored movement

         when PC_Unanchored =>

            --  All done if we tried every position

            if Cursor > Length then
               goto Match_Fail;

            --  Otherwise extend the anchor point, and restack ourself

            else
               Cursor := Cursor + 1;
               Push (Node);
               goto Succeed;
            end if;

         --  Write immediate. This node performs the actual write

         when PC_Write_Imm =>
            Put_Line
              (Node.FP.all,
               Subject (Stack (Stack_Base - 1).Cursor + 1 .. Cursor));
            Pop_Region;
            goto Succeed;

         --  Write on match. This node sets up for the eventual write

         when PC_Write_OnM =>
            Stack (Stack_Base - 1).Node := Node;
            Push (CP_Assign'Access);
            Pop_Region;
            Assign_OnM := True;
            goto Succeed;

      end case;

      --  We are NOT allowed to fall though this case statement, since every
      --  match routine must end by executing a goto to the appropriate point
      --  in the finite state machine model.

      Logic_Error;

   end XMatch;

   -------------
   -- XMatchD --
   -------------

   --  Maintenance note: There is a LOT of code duplication between XMatch
   --  and XMatchD. This is quite intentional, the point is to avoid any
   --  unnecessary debugging overhead in the XMatch case, but this does mean
   --  that any changes to XMatchD must be mirrored in XMatch. In case of
   --  any major changes, the proper approach is to delete XMatch, make the
   --  changes to XMatchD, and then make a copy of XMatchD, removing all
   --  calls to Dout, and all Put and Put_Line operations. This copy becomes
   --  the new XMatch.

   procedure XMatchD
     (Subject : String;
      Pat_P   : PE_Ptr;
      Pat_S   : Natural;
      Start   : out Natural;
      Stop    : out Natural)
   is
      Node : PE_Ptr;
      --  Pointer to current pattern node. Initialized from Pat_P, and then
      --  updated as the match proceeds through its constituent elements.

      Length : constant Natural := Subject'Length;
      --  Length of string (= Subject'Last, since Subject'First is always 1)

      Cursor : Integer := 0;
      --  If the value is non-negative, then this value is the index showing
      --  the current position of the match in the subject string. The next
      --  character to be matched is at Subject (Cursor + 1). Note that since
      --  our view of the subject string in XMatch always has a lower bound
      --  of one, regardless of original bounds, that this definition exactly
      --  corresponds to the cursor value as referenced by functions like Pos.
      --
      --  If the value is negative, then this is a saved stack pointer,
      --  typically a base pointer of an inner or outer region. Cursor
      --  temporarily holds such a value when it is popped from the stack
      --  by Fail. In all cases, Cursor is reset to a proper non-negative
      --  cursor value before the match proceeds (e.g. by propagating the
      --  failure and popping a "real" cursor value from the stack.

      PE_Unanchored : aliased PE := (PC_Unanchored, 0, Pat_P);
      --  Dummy pattern element used in the unanchored case.

      Region_Level : Natural := 0;
      --  Keeps track of recursive region level. This is used only for
      --  debugging, it is the number of saved history stack base values.

      Stack : Stack_Type;
      --  The pattern matching failure stack for this call to Match

      Stack_Ptr : Stack_Range;
      --  Current stack pointer. This points to the top element of the stack
      --  that is currently in use. At the outer level this is the special
      --  entry placed on the stack according to the anchor mode.

      Stack_Init : constant Stack_Range := Stack'First + 1;
      --  This is the initial value of the Stack_Ptr and Stack_Base. The
      --  initial (Stack'First) element of the stack is not used so that
      --  when we pop the last element off, Stack_Ptr is still in range.

      Stack_Base : Stack_Range;
      --  This value is the stack base value, i.e. the stack pointer for the
      --  first history stack entry in the current stack region. See separate
      --  section on handling of recursive pattern matches.

      Assign_OnM : Boolean := False;
      --  Set True if assign-on-match or write-on-match operations may be
      --  present in the history stack, which must then be scanned on a
      --  successful match.

      procedure Dout (Str : String);
      --  Output string to standard error with bars indicating region level.

      procedure Dout (Str : String; A : Character);
      --  Calls Dout with the string S ('A')

      procedure Dout (Str : String; A : Character_Set);
      --  Calls Dout with the string S ("A")

      procedure Dout (Str : String; A : Natural);
      --  Calls Dout with the string S (A)

      procedure Dout (Str : String; A : String);
      --  Calls Dout with the string S ("A")

      function Img (P : PE_Ptr) return String;
      --  Returns a string of the form #nnn where nnn is P.Index

      procedure Pop_Region;
      pragma Inline (Pop_Region);
      --  Used at the end of processing of an inner region. if the inner
      --  region left no stack entries, then all trace of it is removed.
      --  Otherwise a PC_Restore_Region entry is pushed to ensure proper
      --  handling of alternatives in the inner region.

      procedure Push (Node : PE_Ptr);
      pragma Inline (Push);
      --  Make entry in pattern matching stack with current cursor valeu

      procedure Push_Region;
      pragma Inline (Push_Region);
      --  This procedure makes a new region on the history stack. The
      --  caller first establishes the special entry on the stack, but
      --  does not push the stack pointer. Then this call stacks a
      --  PC_Remove_Region node, on top of this entry, using the cursor
      --  field of the PC_Remove_Region entry to save the outer level
      --  stack base value, and resets the stack base to point to this
      --  PC_Remove_Region node.

      ----------
      -- Dout --
      ----------

      procedure Dout (Str : String) is
      begin
         for J in 1 .. Region_Level loop
            Put ("| ");
         end loop;

         Put_Line (Str);
      end Dout;

      procedure Dout (Str : String; A : Character) is
      begin
         Dout (Str & " ('" & A & "')");
      end Dout;

      procedure Dout (Str : String; A : Character_Set) is
      begin
         Dout (Str & " (" & Image (To_Sequence (A)) & ')');
      end Dout;

      procedure Dout (Str : String; A : Natural) is
      begin
         Dout (Str & " (" & A & ')');
      end Dout;

      procedure Dout (Str : String; A : String) is
      begin
         Dout (Str & " (" & Image (A) & ')');
      end Dout;

      ---------
      -- Img --
      ---------

      function Img (P : PE_Ptr) return String is
      begin
         return "#" & Integer (P.Index) & " ";
      end Img;

      ----------------
      -- Pop_Region --
      ----------------

      procedure Pop_Region is
      begin
         Region_Level := Region_Level - 1;

         --  If nothing was pushed in the inner region, we can just get
         --  rid of it entirely, leaving no traces that it was ever there

         if Stack_Ptr = Stack_Base then
            Stack_Ptr := Stack_Base - 2;
            Stack_Base := Stack (Stack_Ptr + 2).Cursor;

         --  If stuff was pushed in the inner region, then we have to
         --  push a PC_R_Restore node so that we properly handle possible
         --  rematches within the region.

         else
            Stack_Ptr := Stack_Ptr + 1;
            Stack (Stack_Ptr).Cursor := Stack_Base;
            Stack (Stack_Ptr).Node   := CP_R_Restore'Access;
            Stack_Base := Stack (Stack_Base).Cursor;
         end if;
      end Pop_Region;

      ----------
      -- Push --
      ----------

      procedure Push (Node : PE_Ptr) is
      begin
         Stack_Ptr := Stack_Ptr + 1;
         Stack (Stack_Ptr).Cursor := Cursor;
         Stack (Stack_Ptr).Node   := Node;
      end Push;

      -----------------
      -- Push_Region --
      -----------------

      procedure Push_Region is
      begin
         Region_Level := Region_Level + 1;
         Stack_Ptr := Stack_Ptr + 2;
         Stack (Stack_Ptr).Cursor := Stack_Base;
         Stack (Stack_Ptr).Node   := CP_R_Remove'Access;
         Stack_Base := Stack_Ptr;
      end Push_Region;

   --  Start of processing for XMatchD

   begin
      New_Line;
      Put_Line ("Initiating pattern match, subject = " & Image (Subject));
      Put      ("--------------------------------------");

      for J in 1 .. Length loop
         Put ('-');
      end loop;

      New_Line;
      Put_Line ("subject length = " & Length);

      if Pat_P = null then
         Uninitialized_Pattern;
      end if;

      --  Check we have enough stack for this pattern. This check deals with
      --  every possibility except a match of a recursive pattern, where we
      --  make a check at each recursion level.

      if Pat_S >= Stack_Size - 1 then
         raise Pattern_Stack_Overflow;
      end if;

      --  In anchored mode, the bottom entry on the stack is an abort entry

      if Anchored_Mode then
         Stack (Stack_Init).Node   := CP_Cancel'Access;
         Stack (Stack_Init).Cursor := 0;

      --  In unanchored more, the bottom entry on the stack references
      --  the special pattern element PE_Unanchored, whose Pthen field
      --  points to the initial pattern element. The cursor value in this
      --  entry is the number of anchor moves so far.

      else
         Stack (Stack_Init).Node   := PE_Unanchored'Unchecked_Access;
         Stack (Stack_Init).Cursor := 0;
      end if;

      Stack_Ptr    := Stack_Init;
      Stack_Base   := Stack_Ptr;
      Cursor       := 0;
      Node         := Pat_P;
      goto Match;

      -----------------------------------------
      -- Main Pattern Matching State Control --
      -----------------------------------------

      --  This is a state machine which uses gotos to change state. The
      --  initial state is Match, to initiate the matching of the first
      --  element, so the goto Match above starts the match. In the
      --  following descriptions, we indicate the global values that
      --  are relevant for the state transition.

      --  Come here if entire match fails

      <<Match_Fail>>
         Dout ("match fails");
         New_Line;
         Start := 0;
         Stop  := 0;
         return;

      --  Come here if entire match succeeds

      --    Cursor        current position in subject string

      <<Match_Succeed>>
         Dout ("match succeeds");
         Start := Stack (Stack_Init).Cursor + 1;
         Stop  := Cursor;
         Dout ("first matched character index = " & Start);
         Dout ("last matched character index = " & Stop);
         Dout ("matched substring = " & Image (Subject (Start .. Stop)));

         --  Scan history stack for deferred assignments or writes

         if Assign_OnM then
            for S in Stack'First .. Stack_Ptr loop
               if Stack (S).Node = CP_Assign'Access then
                  declare
                     Inner_Base    : constant Stack_Range :=
                                       Stack (S + 1).Cursor;
                     Special_Entry : constant Stack_Range :=
                                       Inner_Base - 1;
                     Node_OnM      : constant PE_Ptr  :=
                                       Stack (Special_Entry).Node;
                     Start         : constant Natural :=
                                       Stack (Special_Entry).Cursor + 1;
                     Stop          : constant Natural := Stack (S).Cursor;

                  begin
                     if Node_OnM.Pcode = PC_Assign_OnM then
                        Set_String (Node_OnM.VP.all, Subject (Start .. Stop));
                        Dout
                          (Img (Stack (S).Node) &
                           "deferred assignment of " &
                           Image (Subject (Start .. Stop)));

                     elsif Node_OnM.Pcode = PC_Write_OnM then
                        Put_Line (Node_OnM.FP.all, Subject (Start .. Stop));
                        Dout
                          (Img (Stack (S).Node) &
                           "deferred write of " &
                           Image (Subject (Start .. Stop)));

                     else
                        Logic_Error;
                     end if;
                  end;
               end if;
            end loop;
         end if;

         New_Line;
         return;

      --  Come here if attempt to match current element fails

      --    Stack_Base    current stack base
      --    Stack_Ptr     current stack pointer

      <<Fail>>
         Cursor := Stack (Stack_Ptr).Cursor;
         Node   := Stack (Stack_Ptr).Node;
         Stack_Ptr := Stack_Ptr - 1;

         if Cursor >= 0 then
            Dout ("failure, cursor reset to " & Cursor);
         end if;

         goto Match;

      --  Come here if attempt to match current element succeeds

      --    Cursor        current position in subject string
      --    Node          pointer to node successfully matched
      --    Stack_Base    current stack base
      --    Stack_Ptr     current stack pointer

      <<Succeed>>
         Dout ("success, cursor = " & Cursor);
         Node := Node.Pthen;

      --  Come here to match the next pattern element

      --    Cursor        current position in subject string
      --    Node          pointer to node to be matched
      --    Stack_Base    current stack base
      --    Stack_Ptr     current stack pointer

      <<Match>>

      --------------------------------------------------
      -- Main Pattern Match Element Matching Routines --
      --------------------------------------------------

      --  Here is the case statement that processes the current node. The
      --  processing for each element does one of five things:

      --    goto Succeed        to move to the successor
      --    goto Match_Succeed  if the entire match succeeds
      --    goto Match_Fail     if the entire match fails
      --    goto Fail           to signal failure of current match

      --  Processing is NOT allowed to fall through

      case Node.Pcode is

         --  Cancel

         when PC_Cancel =>
            Dout (Img (Node) & "matching Cancel");
            goto Match_Fail;

         --  Alternation

         when PC_Alt =>
            Dout
              (Img (Node) & "setting up alternative " & Img (Node.Alt));
            Push (Node.Alt);
            Node := Node.Pthen;
            goto Match;

         --  Any (one character case)

         when PC_Any_CH =>
            Dout (Img (Node) & "matching Any", Node.Char);

            if Cursor < Length
              and then Subject (Cursor + 1) = Node.Char
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Any (character set case)

         when PC_Any_CS =>
            Dout (Img (Node) & "matching Any", Node.CS);

            if Cursor < Length
              and then Is_In (Subject (Cursor + 1), Node.CS)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Any (string function case)

         when PC_Any_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            Dout (Img (Node) & "matching Any", Str.all);

            if Cursor < Length
              and then Is_In (Subject (Cursor + 1), Str.all)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Any (string pointer case)

         when PC_Any_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            Dout (Img (Node) & "matching Any", Str.all);

            if Cursor < Length
              and then Is_In (Subject (Cursor + 1), Str.all)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Arb (initial match)

         when PC_Arb_X =>
            Dout (Img (Node) & "matching Arb");
            Push (Node.Alt);
            Node := Node.Pthen;
            goto Match;

         --  Arb (extension)

         when PC_Arb_Y  =>
            Dout (Img (Node) & "extending Arb");

            if Cursor < Length then
               Cursor := Cursor + 1;
               Push (Node);
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Arbno_S (simple Arbno initialize). This is the node that
         --  initiates the match of a simple Arbno structure.

         when PC_Arbno_S =>
            Dout (Img (Node) &
                  "setting up Arbno alternative " & Img (Node.Alt));
            Push (Node.Alt);
            Node := Node.Pthen;
            goto Match;

         --  Arbno_X (Arbno initialize). This is the node that initiates
         --  the match of a complex Arbno structure.

         when PC_Arbno_X =>
            Dout (Img (Node) &
                  "setting up Arbno alternative " & Img (Node.Alt));
            Push (Node.Alt);
            Node := Node.Pthen;
            goto Match;

         --  Arbno_Y (Arbno rematch). This is the node that is executed
         --  following successful matching of one instance of a complex
         --  Arbno pattern.

         when PC_Arbno_Y => declare
            Null_Match : Boolean := (Cursor = Stack (Stack_Base - 1).Cursor);

         begin
            Dout (Img (Node) & "extending Arbno");
            Pop_Region;

            --  If arbno extension matched null, then immediately fail

            if Null_Match then
               Dout ("Arbno extension matched null, so fails");
               goto Fail;
            end if;

            --  Here we must do a stack check to make sure enough stack
            --  is left. This check will happen once for each instance of
            --  the Arbno pattern that is matched. The Nat field of a
            --  PC_Arbno pattern contains the maximum stack entries needed
            --  for the Arbno with one instance and the successor pattern

            if Stack_Ptr + Node.Nat >= Stack'Last then
               raise Pattern_Stack_Overflow;
            end if;

            goto Succeed;
         end;

         --  Assign. If this node is executed, it means the assign-on-match
         --  or write-on-match operation will not happen after all, so we
         --  is propagate the failure, removing the PC_Assign node.

         when PC_Assign =>
            Dout (Img (Node) & "deferred assign/write cancelled");
            goto Fail;

         --  Assign immediate. This node performs the actual assignment.

         when PC_Assign_Imm =>
            Dout
              (Img (Node) & "executing immediate assignment of " &
               Image (Subject (Stack (Stack_Base - 1).Cursor + 1 .. Cursor)));
            Set_String
              (Node.VP.all,
               Subject (Stack (Stack_Base - 1).Cursor + 1 .. Cursor));
            Pop_Region;
            goto Succeed;

         --  Assign on match. This node sets up for the eventual assignment

         when PC_Assign_OnM =>
            Dout (Img (Node) & "registering deferred assignment");
            Stack (Stack_Base - 1).Node := Node;
            Push (CP_Assign'Access);
            Pop_Region;
            Assign_OnM := True;
            goto Succeed;

         --  Bal

         when PC_Bal =>
            Dout (Img (Node) & "matching or extending Bal");
            if Cursor >= Length or else Subject (Cursor + 1) = ')' then
               goto Fail;

            elsif Subject (Cursor + 1) = '(' then
               declare
                  Paren_Count : Natural := 1;

               begin
                  loop
                     Cursor := Cursor + 1;

                     if Cursor >= Length then
                        goto Fail;

                     elsif Subject (Cursor + 1) = '(' then
                        Paren_Count := Paren_Count + 1;

                     elsif Subject (Cursor + 1) = ')' then
                        Paren_Count := Paren_Count - 1;
                        exit when Paren_Count = 0;
                     end if;
                  end loop;
               end;
            end if;

            Cursor := Cursor + 1;
            Push (Node);
            goto Succeed;

         --  Break (one character case)

         when PC_Break_CH =>
            Dout (Img (Node) & "matching Break", Node.Char);

            while Cursor < Length loop
               if Subject (Cursor + 1) = Node.Char then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;

         --  Break (character set case)

         when PC_Break_CS =>
            Dout (Img (Node) & "matching Break", Node.CS);

            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Node.CS) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;

         --  Break (string function case)

         when PC_Break_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            Dout (Img (Node) & "matching Break", Str.all);

            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Str.all) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;
         end;

         --  Break (string pointer case)

         when PC_Break_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            Dout (Img (Node) & "matching Break", Str.all);

            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Str.all) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;
         end;

         --  BreakX (one character case)

         when PC_BreakX_CH =>
            Dout (Img (Node) & "matching BreakX", Node.Char);

            while Cursor < Length loop
               if Subject (Cursor + 1) = Node.Char then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;

         --  BreakX (character set case)

         when PC_BreakX_CS =>
            Dout (Img (Node) & "matching BreakX", Node.CS);

            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Node.CS) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;

         --  BreakX (string function case)

         when PC_BreakX_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            Dout (Img (Node) & "matching BreakX", Str.all);

            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Str.all) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;
         end;

         --  BreakX (string pointer case)

         when PC_BreakX_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            Dout (Img (Node) & "matching BreakX", Str.all);

            while Cursor < Length loop
               if Is_In (Subject (Cursor + 1), Str.all) then
                  goto Succeed;
               else
                  Cursor := Cursor + 1;
               end if;
            end loop;

            goto Fail;
         end;

         --  BreakX_X (BreakX extension). See section on "Compound Pattern
         --  Structures". This node is the alternative that is stacked
         --  to skip past the break character and extend the break.

         when PC_BreakX_X =>
            Dout (Img (Node) & "extending BreakX");

            Cursor := Cursor + 1;
            goto Succeed;

         --  Character (one character string)

         when PC_Char =>
            Dout (Img (Node) & "matching '" & Node.Char & ''');

            if Cursor < Length
              and then Subject (Cursor + 1) = Node.Char
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  End of Pattern

         when PC_EOP =>
            if Stack_Base = Stack_Init then
               Dout ("end of pattern");
               goto Match_Succeed;

            --  End of recursive inner match. See separate section on
            --  handing of recursive pattern matches for details.

            else
               Dout ("terminating recursive match");
               Node := Stack (Stack_Base - 1).Node;
               Pop_Region;
               goto Match;
            end if;

         --  Fail

         when PC_Fail =>
            Dout (Img (Node) & "matching Fail");
            goto Fail;

         --  Fence (built in pattern)

         when PC_Fence =>
            Dout (Img (Node) & "matching Fence");
            Push (CP_Cancel'Access);
            goto Succeed;

         --  Fence function node X. This is the node that gets control
         --  after a successful match of the fenced pattern.

         when PC_Fence_X =>
            Dout (Img (Node) & "matching Fence function");
            Stack_Ptr := Stack_Ptr + 1;
            Stack (Stack_Ptr).Cursor := Stack_Base;
            Stack (Stack_Ptr).Node   := CP_Fence_Y'Access;
            Stack_Base := Stack (Stack_Base).Cursor;
            Region_Level := Region_Level - 1;
            goto Succeed;

         --  Fence function node Y. This is the node that gets control on
         --  a failure that occurs after the fenced pattern has matched.

         --  Note: the Cursor at this stage is actually the inner stack
         --  base value. We don't reset this, but we do use it to strip
         --  off all the entries made by the fenced pattern.

         when PC_Fence_Y =>
            Dout (Img (Node) & "pattern matched by Fence caused failure");
            Stack_Ptr := Cursor - 2;
            goto Fail;

         --  Len (integer case)

         when PC_Len_Nat =>
            Dout (Img (Node) & "matching Len", Node.Nat);

            if Cursor + Node.Nat > Length then
               goto Fail;
            else
               Cursor := Cursor + Node.Nat;
               goto Succeed;
            end if;

         --  Len (Integer function case)

         when PC_Len_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            Dout (Img (Node) & "matching Len", N);

            if Cursor + N > Length then
               goto Fail;
            else
               Cursor := Cursor + N;
               goto Succeed;
            end if;
         end;

         --  Len (integer pointer case)

         when PC_Len_NP =>
            Dout (Img (Node) & "matching Len", Node.NP.all);

            if Cursor + Node.NP.all > Length then
               goto Fail;
            else
               Cursor := Cursor + Node.NP.all;
               goto Succeed;
            end if;

         --  NotAny (one character case)

         when PC_NotAny_CH =>
            Dout (Img (Node) & "matching NotAny", Node.Char);

            if Cursor < Length
              and then Subject (Cursor + 1) /= Node.Char
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  NotAny (character set case)

         when PC_NotAny_CS =>
            Dout (Img (Node) & "matching NotAny", Node.CS);

            if Cursor < Length
              and then not Is_In (Subject (Cursor + 1), Node.CS)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  NotAny (string function case)

         when PC_NotAny_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            Dout (Img (Node) & "matching NotAny", Str.all);

            if Cursor < Length
              and then
                not Is_In (Subject (Cursor + 1), Str.all)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  NotAny (string pointer case)

         when PC_NotAny_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            Dout (Img (Node) & "matching NotAny", Str.all);

            if Cursor < Length
              and then
                not Is_In (Subject (Cursor + 1), Str.all)
            then
               Cursor := Cursor + 1;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  NSpan (one character case)

         when PC_NSpan_CH =>
            Dout (Img (Node) & "matching NSpan", Node.Char);

            while Cursor < Length
              and then Subject (Cursor + 1) = Node.Char
            loop
               Cursor := Cursor + 1;
            end loop;

            goto Succeed;

         --  NSpan (character set case)

         when PC_NSpan_CS =>
            Dout (Img (Node) & "matching NSpan", Node.CS);

            while Cursor < Length
              and then Is_In (Subject (Cursor + 1), Node.CS)
            loop
               Cursor := Cursor + 1;
            end loop;

            goto Succeed;

         --  NSpan (string function case)

         when PC_NSpan_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);

         begin
            Dout (Img (Node) & "matching NSpan", Str.all);

            while Cursor < Length
              and then Is_In (Subject (Cursor + 1), Str.all)
            loop
               Cursor := Cursor + 1;
            end loop;

            goto Succeed;
         end;

         --  NSpan (string pointer case)

         when PC_NSpan_VP => declare
            Str : String_Access := Get_String (Node.VP.all);

         begin
            Dout (Img (Node) & "matching NSpan", Str.all);

            while Cursor < Length
              and then Is_In (Subject (Cursor + 1), Str.all)
            loop
               Cursor := Cursor + 1;
            end loop;

            goto Succeed;
         end;

         when PC_Null =>
            Dout (Img (Node) & "matching null");
            goto Succeed;

         --  Pos (integer case)

         when PC_Pos_Nat =>
            Dout (Img (Node) & "matching Pos", Node.Nat);

            if Cursor = Node.Nat then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Pos (Integer function case)

         when PC_Pos_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            Dout (Img (Node) & "matching Pos", N);

            if Cursor = N then
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Pos (integer pointer case)

         when PC_Pos_NP =>
            Dout (Img (Node) & "matching Pos", Node.NP.all);

            if Cursor = Node.NP.all then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Predicate function

         when PC_Pred_Func =>
            Dout (Img (Node) & "matching predicate function");

            if Node.BF.all then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Region Enter. Initiate new pattern history stack region

         when PC_R_Enter =>
            Dout (Img (Node) & "starting match of nested pattern");
            Stack (Stack_Ptr + 1).Cursor := Cursor;
            Push_Region;
            goto Succeed;

         --  Region Remove node. This is the node stacked by an R_Enter.
         --  It removes the special format stack entry right underneath, and
         --  then restores the outer level stack base and signals failure.

         --  Note: the cursor value at this stage is actually the (negative)
         --  stack base value for the outer level.

         when PC_R_Remove =>
            Dout ("failure, match of nested pattern terminated");
            Stack_Base := Cursor;
            Region_Level := Region_Level - 1;
            Stack_Ptr := Stack_Ptr - 1;
            goto Fail;

         --  Region restore node. This is the node stacked at the end of an
         --  inner level match. Its function is to restore the inner level
         --  region, so that alternatives in this region can be sought.

         --  Note: the Cursor at this stage is actually the negative of the
         --  inner stack base value, which we use to restore the inner region.

         when PC_R_Restore =>
            Dout ("failure, search for alternatives in nested pattern");
            Region_Level := Region_Level + 1;
            Stack_Base := Cursor;
            goto Fail;

         --  Rest

         when PC_Rest =>
            Dout (Img (Node) & "matching Rest");
            Cursor := Length;
            goto Succeed;

         --  Initiate recursive match (pattern pointer case)

         when PC_Rpat =>
            Stack (Stack_Ptr + 1).Node := Node.Pthen;
            Push_Region;
            Dout (Img (Node) & "initiating recursive match");

            if Stack_Ptr + Node.PP.all.Stk >= Stack_Size then
               raise Pattern_Stack_Overflow;
            else
               Node := Node.PP.all.P;
               goto Match;
            end if;

         --  RPos (integer case)

         when PC_RPos_Nat =>
            Dout (Img (Node) & "matching RPos", Node.Nat);

            if Cursor = (Length - Node.Nat) then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  RPos (integer function case)

         when PC_RPos_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            Dout (Img (Node) & "matching RPos", N);

            if Length - Cursor = N then
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  RPos (integer pointer case)

         when PC_RPos_NP =>
            Dout (Img (Node) & "matching RPos", Node.NP.all);

            if Cursor = (Length - Node.NP.all) then
               goto Succeed;
            else
               goto Fail;
            end if;

         --  RTab (integer case)

         when PC_RTab_Nat =>
            Dout (Img (Node) & "matching RTab", Node.Nat);

            if Cursor <= (Length - Node.Nat) then
               Cursor := Length - Node.Nat;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  RTab (integer function case)

         when PC_RTab_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            Dout (Img (Node) & "matching RPos", N);

            if Length - Cursor >= N then
               Cursor := Length - N;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  RTab (integer pointer case)

         when PC_RTab_NP =>
            Dout (Img (Node) & "matching RPos", Node.NP.all);

            if Cursor <= (Length - Node.NP.all) then
               Cursor := Length - Node.NP.all;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Cursor assignment

         when PC_Setcur =>
            Dout (Img (Node) & "matching Setcur");
            Node.Var.all := Cursor;
            goto Succeed;

         --  Span (one character case)

         when PC_Span_CH => declare
            P : Natural := Cursor;

         begin
            Dout (Img (Node) & "matching Span", Node.Char);

            while P < Length
              and then Subject (P + 1) = Node.Char
            loop
               P := P + 1;
            end loop;

            if P /= Cursor then
               Cursor := P;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Span (character set case)

         when PC_Span_CS => declare
            P : Natural := Cursor;

         begin
            Dout (Img (Node) & "matching Span", Node.CS);

            while P < Length
              and then Is_In (Subject (P + 1), Node.CS)
            loop
               P := P + 1;
            end loop;

            if P /= Cursor then
               Cursor := P;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Span (string function case)

         when PC_Span_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);
            P   : Natural := Cursor;

         begin
            Dout (Img (Node) & "matching Span", Str.all);

            while P < Length
              and then Is_In (Subject (P + 1), Str.all)
            loop
               P := P + 1;
            end loop;

            if P /= Cursor then
               Cursor := P;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Span (string pointer case)

         when PC_Span_VP => declare
            Str : String_Access := Get_String (Node.VP.all);
            P   : Natural := Cursor;

         begin
            Dout (Img (Node) & "matching Span", Str.all);

            while P < Length
              and then Is_In (Subject (P + 1), Str.all)
            loop
               P := P + 1;
            end loop;

            if P /= Cursor then
               Cursor := P;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  String (two character case)

         when PC_String_2 =>
            Dout (Img (Node) & "matching " & Image (Node.Str2));

            if (Length - Cursor) >= 2
              and then Subject (Cursor + 1 .. Cursor + 2) = Node.Str2
            then
               Cursor := Cursor + 2;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (three character case)

         when PC_String_3 =>
            Dout (Img (Node) & "matching " & Image (Node.Str3));

            if (Length - Cursor) >= 3
              and then Subject (Cursor + 1 .. Cursor + 3) = Node.Str3
            then
               Cursor := Cursor + 3;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (four character case)

         when PC_String_4 =>
            Dout (Img (Node) & "matching " & Image (Node.Str4));

            if (Length - Cursor) >= 4
              and then Subject (Cursor + 1 .. Cursor + 4) = Node.Str4
            then
               Cursor := Cursor + 4;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (five character case)

         when PC_String_5 =>
            Dout (Img (Node) & "matching " & Image (Node.Str5));

            if (Length - Cursor) >= 5
              and then Subject (Cursor + 1 .. Cursor + 5) = Node.Str5
            then
               Cursor := Cursor + 5;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (six character case)

         when PC_String_6 =>
            Dout (Img (Node) & "matching " & Image (Node.Str6));

            if (Length - Cursor) >= 6
              and then Subject (Cursor + 1 .. Cursor + 6) = Node.Str6
            then
               Cursor := Cursor + 6;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  String (case of more than six characters)

         when PC_String => declare
            Len : constant Natural := Node.Str'Length;

         begin
            Dout (Img (Node) & "matching " & Image (Node.Str.all));

            if (Length - Cursor) >= Len
              and then Node.Str.all = Subject (Cursor + 1 .. Cursor + Len)
            then
               Cursor := Cursor + Len;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  String (function case)

         when PC_String_VF => declare
            U   : constant VString       := Node.VF.all;
            Str : constant String_Access := Get_String (U);
            Len : constant Natural       := Str'Length;

         begin
            Dout (Img (Node) & "matching " & Image (Str.all));

            if (Length - Cursor) >= Len
              and then Str.all = Subject (Cursor + 1 .. Cursor + Len)
            then
               Cursor := Cursor + Len;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  String (vstring pointer case)

         when PC_String_VP => declare
            S   : String_Access := Get_String (Node.VP.all);
            Len : constant Natural :=
                    Ada.Strings.Unbounded.Length (Node.VP.all);

         begin
            Dout
              (Img (Node) & "matching " & Image (S.all));

            if (Length - Cursor) >= Len
              and then S.all = Subject (Cursor + 1 .. Cursor + Len)
            then
               Cursor := Cursor + Len;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Succeed

         when PC_Succeed =>
            Dout (Img (Node) & "matching Succeed");
            Push (Node);
            goto Succeed;

         --  Tab (integer case)

         when PC_Tab_Nat =>
            Dout (Img (Node) & "matching Tab", Node.Nat);

            if Cursor <= Node.Nat then
               Cursor := Node.Nat;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Tab (integer function case)

         when PC_Tab_NF => declare
            N : constant Natural := Node.NF.all;

         begin
            Dout (Img (Node) & "matching Tab ", N);

            if Cursor <= N then
               Cursor := N;
               goto Succeed;
            else
               goto Fail;
            end if;
         end;

         --  Tab (integer pointer case)

         when PC_Tab_NP =>
            Dout (Img (Node) & "matching Tab ", Node.NP.all);

            if Cursor <= Node.NP.all then
               Cursor := Node.NP.all;
               goto Succeed;
            else
               goto Fail;
            end if;

         --  Unanchored movement

         when PC_Unanchored =>
            Dout ("attempting to move anchor point");

            --  All done if we tried every position

            if Cursor > Length then
               goto Match_Fail;

            --  Otherwise extend the anchor point, and restack ourself

            else
               Cursor := Cursor + 1;
               Push (Node);
               goto Succeed;
            end if;

         --  Write immediate. This node performs the actual write

         when PC_Write_Imm =>
            Dout (Img (Node) & "executing immediate write of " &
                   Subject (Stack (Stack_Base - 1).Cursor + 1 .. Cursor));

            Put_Line
              (Node.FP.all,
               Subject (Stack (Stack_Base - 1).Cursor + 1 .. Cursor));
            Pop_Region;
            goto Succeed;

         --  Write on match. This node sets up for the eventual write

         when PC_Write_OnM =>
            Dout (Img (Node) & "registering deferred write");
            Stack (Stack_Base - 1).Node := Node;
            Push (CP_Assign'Access);
            Pop_Region;
            Assign_OnM := True;
            goto Succeed;

      end case;

      --  We are NOT allowed to fall though this case statement, since every
      --  match routine must end by executing a goto to the appropriate point
      --  in the finite state machine model.

      Logic_Error;

   end XMatchD;

end GNAT.Spitbol.Patterns;
