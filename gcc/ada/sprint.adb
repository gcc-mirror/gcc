------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S P R I N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Fname;    use Fname;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Sinput.D; use Sinput.D;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Urealp;   use Urealp;

package body Sprint is
   Current_Source_File : Source_File_Index;
   --  Index of source file whose generated code is being dumped

   Dump_Node : Node_Id := Empty;
   --  This is set to the current node, used for printing line numbers. In
   --  Debug_Generated_Code mode, Dump_Node is set to the current node
   --  requiring Sloc fixup, until Set_Debug_Sloc is called to set the proper
   --  value. The call clears it back to Empty.

   Debug_Sloc : Source_Ptr;
   --  Sloc of first byte of line currently being written if we are
   --  generating a source debug file.

   Dump_Original_Only : Boolean;
   --  Set True if the -gnatdo (dump original tree) flag is set

   Dump_Generated_Only : Boolean;
   --  Set True if the -gnatG (dump generated tree) debug flag is set
   --  or for Print_Generated_Code (-gnatG) or Dump_Generated_Code (-gnatD).

   Dump_Freeze_Null : Boolean;
   --  Set True if freeze nodes and non-source null statements output

   Freeze_Indent : Int := 0;
   --  Keep track of freeze indent level (controls output of blank lines before
   --  procedures within expression freeze actions). Relevant only if we are
   --  not in Dump_Source_Text mode, since in Dump_Source_Text mode we don't
   --  output these blank lines in any case.

   Indent : Int := 0;
   --  Number of columns for current line output indentation

   Indent_Annull_Flag : Boolean := False;
   --  Set True if subsequent Write_Indent call to be ignored, gets reset
   --  by this call, so it is only active to suppress a single indent call.

   Last_Line_Printed : Physical_Line_Number;
   --  This keeps track of the physical line number of the last source line
   --  that has been output. The value is only valid in Dump_Source_Text mode.

   Line_Limit : constant := 72;
   --  Limit value for chopping long lines

   -------------------------------
   -- Operator Precedence Table --
   -------------------------------

   --  This table is used to decide whether a subexpression needs to be
   --  parenthesized. The rule is that if an operand of an operator (which
   --  for this purpose includes AND THEN and OR ELSE) is itself an operator
   --  with a lower precedence than the operator (or equal precedence if
   --  appearing as the right operand), then parentheses are required.

   Op_Prec : constant array (N_Subexpr) of Short_Short_Integer :=
               (N_Op_And          => 1,
                N_Op_Or           => 1,
                N_Op_Xor          => 1,
                N_And_Then        => 1,
                N_Or_Else         => 1,

                N_In              => 2,
                N_Not_In          => 2,
                N_Op_Eq           => 2,
                N_Op_Ge           => 2,
                N_Op_Gt           => 2,
                N_Op_Le           => 2,
                N_Op_Lt           => 2,
                N_Op_Ne           => 2,

                N_Op_Add          => 3,
                N_Op_Concat       => 3,
                N_Op_Subtract     => 3,
                N_Op_Plus         => 3,
                N_Op_Minus        => 3,

                N_Op_Divide       => 4,
                N_Op_Mod          => 4,
                N_Op_Rem          => 4,
                N_Op_Multiply     => 4,

                N_Op_Expon        => 5,
                N_Op_Abs          => 5,
                N_Op_Not          => 5,

                others            => 6);

   procedure Sprint_Left_Opnd (N : Node_Id);
   --  Print left operand of operator, parenthesizing if necessary

   procedure Sprint_Right_Opnd (N : Node_Id);
   --  Print right operand of operator, parenthesizing if necessary

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Col_Check (N : Nat);
   --  Check that at least N characters remain on current line, and if not,
   --  then start an extra line with two characters extra indentation for
   --  continuing text on the next line.

   procedure Extra_Blank_Line;
   --  In some situations we write extra blank lines to separate the generated
   --  code to make it more readable. However, these extra blank lines are not
   --  generated in Dump_Source_Text mode, since there the source text lines
   --  output with preceding blank lines are quite sufficient as separators.
   --  This procedure writes a blank line if Dump_Source_Text is False.

   procedure Indent_Annull;
   --  Causes following call to Write_Indent to be ignored. This is used when
   --  a higher level node wants to stop a lower level node from starting a
   --  new line, when it would otherwise be inclined to do so (e.g. the case
   --  of an accept statement called from an accept alternative with a guard)

   procedure Indent_Begin;
   --  Increase indentation level

   procedure Indent_End;
   --  Decrease indentation level

   procedure Note_Implicit_Run_Time_Call (N : Node_Id);
   --  N is the Name field of a function call or procedure statement call.
   --  The effect of the call is to output a $ if the call is identified as
   --  an implicit call to a run time routine.

   procedure Print_Debug_Line (S : String);
   --  Used to print output lines in Debug_Generated_Code mode (this is used
   --  as the argument for a call to Set_Special_Output in package Output).

   procedure Process_TFAI_RR_Flags (Nod : Node_Id);
   --  Given a divide, multiplication or division node, check the flags
   --  Treat_Fixed_As_Integer and Rounded_Flags, and if set, output the
   --  appropriate special syntax characters (# and @).

   procedure Set_Debug_Sloc;
   --  If Dump_Node is non-empty, this routine sets the appropriate value
   --  in its Sloc field, from the current location in the debug source file
   --  that is currently being written.

   procedure Sprint_And_List (List : List_Id);
   --  Print the given list with items separated by vertical "and"

   procedure Sprint_Bar_List (List : List_Id);
   --  Print the given list with items separated by vertical bars

   procedure Sprint_End_Label
     (Node    : Node_Id;
      Default : Node_Id);
   --  Print the end label for a Handled_Sequence_Of_Statements in a body.
   --  If there is not end label, use the defining identifier of the enclosing
   --  construct. If the end label is present, treat it as a reference to the
   --  defining entity of the construct: this guarantees that it carries the
   --  proper sloc information for debugging purposes.

   procedure Sprint_Node_Actual (Node : Node_Id);
   --  This routine prints its node argument. It is a lower level routine than
   --  Sprint_Node, in that it does not bother about rewritten trees.

   procedure Sprint_Node_Sloc (Node : Node_Id);
   --  Like Sprint_Node, but in addition, in Debug_Generated_Code mode,
   --  sets the Sloc of the current debug node to be a copy of the Sloc
   --  of the sprinted node Node. Note that this is done after printing
   --  Node, so that the Sloc is the proper updated value for the debug file.

   procedure Update_Itype (Node : Node_Id);
   --  Update the Sloc of an itype that is not attached to the tree, when
   --  debugging expanded code. This routine is called from nodes whose
   --  type can be an Itype, such as defining_identifiers that may be of
   --  an anonymous access type, or ranges in slices.

   procedure Write_Char_Sloc (C : Character);
   --  Like Write_Char, except that if C is non-blank, Set_Debug_Sloc is
   --  called to ensure that the current node has a proper Sloc set.

   procedure Write_Condition_And_Reason (Node : Node_Id);
   --  Write Condition and Reason codes of Raise_xxx_Error node

   procedure Write_Corresponding_Source (S : String);
   --  If S is a string with a single keyword (possibly followed by a space),
   --  and if the next non-comment non-blank source line matches this keyword,
   --  then output all source lines up to this matching line.

   procedure Write_Discr_Specs (N : Node_Id);
   --  Output discriminant specification for node, which is any of the type
   --  declarations that can have discriminants.

   procedure Write_Ekind (E : Entity_Id);
   --  Write the String corresponding to the Ekind without "E_"

   procedure Write_Id (N : Node_Id);
   --  N is a node with a Chars field. This procedure writes the name that
   --  will be used in the generated code associated with the name. For a
   --  node with no associated entity, this is simply the Chars field. For
   --  the case where there is an entity associated with the node, we print
   --  the name associated with the entity (since it may have been encoded).
   --  One other special case is that an entity has an active external name
   --  (i.e. an external name present with no address clause), then this
   --  external name is output. This procedure also deals with outputting
   --  declarations of referenced itypes, if not output earlier.

   function Write_Identifiers (Node : Node_Id) return Boolean;
   --  Handle node where the grammar has a list of defining identifiers, but
   --  the tree has a separate declaration for each identifier. Handles the
   --  printing of the defining identifier, and returns True if the type and
   --  initialization information is to be printed, False if it is to be
   --  skipped (the latter case happens when printing defining identifiers
   --  other than the first in the original tree output case).

   procedure Write_Implicit_Def (E : Entity_Id);
   pragma Warnings (Off, Write_Implicit_Def);
   --  Write the definition of the implicit type E according to its Ekind
   --  For now a debugging procedure, but might be used in the future.

   procedure Write_Indent;
   --  Start a new line and write indentation spacing

   function Write_Indent_Identifiers (Node : Node_Id) return Boolean;
   --  Like Write_Identifiers except that each new printed declaration
   --  is at the start of a new line.

   function Write_Indent_Identifiers_Sloc (Node : Node_Id) return Boolean;
   --  Like Write_Indent_Identifiers except that in Debug_Generated_Code
   --  mode, the Sloc of the current debug node is set to point to the
   --  first output identifier.

   procedure Write_Indent_Str (S : String);
   --  Start a new line and write indent spacing followed by given string

   procedure Write_Indent_Str_Sloc (S : String);
   --  Like Write_Indent_Str, but in addition, in Debug_Generated_Code mode,
   --  the Sloc of the current node is set to the first non-blank character
   --  in the string S.

   procedure Write_Itype (Typ : Entity_Id);
   --  If Typ is an Itype that has not been written yet, write it. If Typ is
   --  any other kind of entity or tree node, the call is ignored.

   procedure Write_Name_With_Col_Check (N : Name_Id);
   --  Write name (using Write_Name) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Name_With_Col_Check_Sloc (N : Name_Id);
   --  Like Write_Name_With_Col_Check but in addition, in Debug_Generated_Code
   --  mode, sets Sloc of current debug node to first character of name.

   procedure Write_Operator (N : Node_Id; S : String);
   --  Like Write_Str_Sloc, used for operators, encloses the string in
   --  characters {} if the Do_Overflow flag is set on the node N.

   procedure Write_Param_Specs (N : Node_Id);
   --  Output parameter specifications for node (which is either a function
   --  or procedure specification with a Parameter_Specifications field)

   procedure Write_Rewrite_Str (S : String);
   --  Writes out a string (typically containing <<< or >>>}) for a node
   --  created by rewriting the tree. Suppressed if we are outputting the
   --  generated code only, since in this case we don't specially mark nodes
   --  created by rewriting).

   procedure Write_Source_Line (L : Physical_Line_Number);
   --  If writing of interspersed source lines is enabled, then write the given
   --  line from the source file, preceded by Eol, then an extra blank line if
   --  the line has at least one blank, is not a comment and is not line one,
   --  then "--" and the line number followed by period followed by text of the
   --  source line (without terminating Eol). If interspersed source line
   --  output not enabled, then the call has no effect.

   procedure Write_Source_Lines (L : Physical_Line_Number);
   --  If writing of interspersed source lines is enabled, then writes source
   --  lines Last_Line_Printed + 1 .. L, and updates Last_Line_Printed. If
   --  interspersed source line output not enabled, then call has no effect.

   procedure Write_Str_Sloc (S : String);
   --  Like Write_Str, but sets debug Sloc of current debug node to first
   --  non-blank character if a current debug node is active.

   procedure Write_Str_With_Col_Check (S : String);
   --  Write string (using Write_Str) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Str_With_Col_Check_Sloc (S : String);
   --  Like Write_Str_With_Col_Check, but sets debug Sloc of current debug
   --  node to first non-blank character if a current debug node is active.

   procedure Write_Uint_With_Col_Check (U : Uint; Format : UI_Format);
   --  Write Uint (using UI_Write) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.
   --  The format parameter determines the output format (see UI_Write).

   procedure Write_Uint_With_Col_Check_Sloc (U : Uint; Format : UI_Format);
   --  Write Uint (using UI_Write) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.
   --  The format parameter determines the output format (see UI_Write).
   --  In addition, in Debug_Generated_Code mode, sets the current node
   --  Sloc to the first character of the output value.

   procedure Write_Ureal_With_Col_Check_Sloc (U : Ureal);
   --  Write Ureal (using same output format as UR_Write) with column checks
   --  and a possible initial Write_Indent (to get new line) if current line
   --  is too full. In addition, in Debug_Generated_Code mode, sets the
   --  current node Sloc to the first character of the output value.

   ---------------
   -- Col_Check --
   ---------------

   procedure Col_Check (N : Nat) is
   begin
      if N + Column > Line_Limit then
         Write_Indent_Str ("  ");
      end if;
   end Col_Check;

   ----------------------
   -- Extra_Blank_Line --
   ----------------------

   procedure Extra_Blank_Line is
   begin
      if not Dump_Source_Text then
         Write_Indent;
      end if;
   end Extra_Blank_Line;

   -------------------
   -- Indent_Annull --
   -------------------

   procedure Indent_Annull is
   begin
      Indent_Annull_Flag := True;
   end Indent_Annull;

   ------------------
   -- Indent_Begin --
   ------------------

   procedure Indent_Begin is
   begin
      Indent := Indent + 3;
   end Indent_Begin;

   ----------------
   -- Indent_End --
   ----------------

   procedure Indent_End is
   begin
      Indent := Indent - 3;
   end Indent_End;

   ---------------------------------
   -- Note_Implicit_Run_Time_Call --
   ---------------------------------

   procedure Note_Implicit_Run_Time_Call (N : Node_Id) is
   begin
      if not Comes_From_Source (N)
        and then Is_Entity_Name (N)
      then
         declare
            Ent : constant Entity_Id := Entity (N);
         begin
            if not In_Extended_Main_Source_Unit (Ent)
              and then
                Is_Predefined_File_Name
                  (Unit_File_Name (Get_Source_Unit (Ent)))
            then
               Col_Check (Length_Of_Name (Chars (Ent)));
               Write_Char ('$');
            end if;
         end;
      end if;
   end Note_Implicit_Run_Time_Call;

   --------
   -- pg --
   --------

   procedure pg (Arg : Union_Id) is
   begin
      Dump_Generated_Only := True;
      Dump_Original_Only := False;
      Current_Source_File := No_Source_File;

      if Arg in List_Range then
         Sprint_Node_List (List_Id (Arg));

      elsif Arg in Node_Range then
         Sprint_Node (Node_Id (Arg));

      else
         null;
      end if;

      Write_Eol;
   end pg;

   --------
   -- po --
   --------

   procedure po (Arg : Union_Id) is
   begin
      Dump_Generated_Only := False;
      Dump_Original_Only := True;
      Current_Source_File := No_Source_File;

      if Arg in List_Range then
         Sprint_Node_List (List_Id (Arg));

      elsif Arg in Node_Range then
         Sprint_Node (Node_Id (Arg));

      else
         null;
      end if;

      Write_Eol;
   end po;

   ----------------------
   -- Print_Debug_Line --
   ----------------------

   procedure Print_Debug_Line (S : String) is
   begin
      Write_Debug_Line (S, Debug_Sloc);
   end Print_Debug_Line;

   ---------------------------
   -- Process_TFAI_RR_Flags --
   ---------------------------

   procedure Process_TFAI_RR_Flags (Nod : Node_Id) is
   begin
      if Treat_Fixed_As_Integer (Nod) then
         Write_Char ('#');
      end if;

      if Rounded_Result (Nod) then
         Write_Char ('@');
      end if;
   end Process_TFAI_RR_Flags;

   --------
   -- ps --
   --------

   procedure ps (Arg : Union_Id) is
   begin
      Dump_Generated_Only := False;
      Dump_Original_Only := False;
      Current_Source_File := No_Source_File;

      if Arg in List_Range then
         Sprint_Node_List (List_Id (Arg));

      elsif Arg in Node_Range then
         Sprint_Node (Node_Id (Arg));

      else
         null;
      end if;

      Write_Eol;
   end ps;

   --------------------
   -- Set_Debug_Sloc --
   --------------------

   procedure Set_Debug_Sloc is
   begin
      if Debug_Generated_Code and then Present (Dump_Node) then
         Set_Sloc (Dump_Node, Debug_Sloc + Source_Ptr (Column - 1));
         Dump_Node := Empty;
      end if;
   end Set_Debug_Sloc;

   -----------------
   -- Source_Dump --
   -----------------

   procedure Source_Dump is

      procedure Underline;
      --  Put underline under string we just printed

      ---------------
      -- Underline --
      ---------------

      procedure Underline is
         Col : constant Int := Column;

      begin
         Write_Eol;

         while Col > Column loop
            Write_Char ('-');
         end loop;

         Write_Eol;
      end Underline;

   --  Start of processing for Tree_Dump

   begin
      Dump_Generated_Only := Debug_Flag_G or
                             Print_Generated_Code or
                             Debug_Generated_Code;
      Dump_Original_Only  := Debug_Flag_O;
      Dump_Freeze_Null    := Debug_Flag_S or Debug_Flag_G;

      --  Note that we turn off the tree dump flags immediately, before
      --  starting the dump. This avoids generating two copies of the dump
      --  if an abort occurs after printing the dump, and more importantly,
      --  avoids an infinite loop if an abort occurs during the dump.

      if Debug_Flag_Z then
         Current_Source_File := No_Source_File;
         Debug_Flag_Z := False;
         Write_Eol;
         Write_Eol;
         Write_Str ("Source recreated from tree of Standard (spec)");
         Underline;
         Sprint_Node (Standard_Package_Node);
         Write_Eol;
         Write_Eol;
      end if;

      if Debug_Flag_S or Dump_Generated_Only or Dump_Original_Only then
         Debug_Flag_G := False;
         Debug_Flag_O := False;
         Debug_Flag_S := False;

         --  Dump requested units

         for U in Main_Unit .. Last_Unit loop
            Current_Source_File := Source_Index (U);

            --  Dump all units if -gnatdf set, otherwise we dump only
            --  the source files that are in the extended main source.

            if Debug_Flag_F
              or else In_Extended_Main_Source_Unit (Cunit_Entity (U))
            then
               --  If we are generating debug files, setup to write them

               if Debug_Generated_Code then
                  Set_Special_Output (Print_Debug_Line'Access);
                  Create_Debug_Source (Source_Index (U), Debug_Sloc);
                  Write_Source_Line (1);
                  Last_Line_Printed := 1;
                  Sprint_Node (Cunit (U));
                  Write_Source_Lines (Last_Source_Line (Current_Source_File));
                  Write_Eol;
                  Close_Debug_Source;
                  Set_Special_Output (null);

               --  Normal output to standard output file

               else
                  Write_Str ("Source recreated from tree for ");
                  Write_Unit_Name (Unit_Name (U));
                  Underline;
                  Write_Source_Line (1);
                  Last_Line_Printed := 1;
                  Sprint_Node (Cunit (U));
                  Write_Source_Lines (Last_Source_Line (Current_Source_File));
                  Write_Eol;
                  Write_Eol;
               end if;
            end if;
         end loop;
      end if;
   end Source_Dump;

   ---------------------
   -- Sprint_And_List --
   ---------------------

   procedure Sprint_And_List (List : List_Id) is
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Sprint_Node (Node);
            Next (Node);
            exit when Node = Empty;
            Write_Str (" and ");
         end loop;
      end if;
   end Sprint_And_List;

   ---------------------
   -- Sprint_Bar_List --
   ---------------------

   procedure Sprint_Bar_List (List : List_Id) is
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Sprint_Node (Node);
            Next (Node);
            exit when Node = Empty;
            Write_Str (" | ");
         end loop;
      end if;
   end Sprint_Bar_List;

   ----------------------
   -- Sprint_End_Label --
   ----------------------

   procedure Sprint_End_Label
     (Node    : Node_Id;
      Default : Node_Id)
   is
   begin
      if Present (Node)
        and then Present (End_Label (Node))
        and then Is_Entity_Name (End_Label (Node))
      then
         Set_Entity (End_Label (Node), Default);

         --  For a function whose name is an operator, use the qualified name
         --  created for the defining entity.

         if Nkind (End_Label (Node)) = N_Operator_Symbol then
            Set_Chars (End_Label (Node), Chars (Default));
         end if;

         Sprint_Node (End_Label (Node));
      else
         Sprint_Node (Default);
      end if;
   end Sprint_End_Label;

   -----------------------
   -- Sprint_Comma_List --
   -----------------------

   procedure Sprint_Comma_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Sprint_Node (Node);
            Next (Node);
            exit when Node = Empty;

            if not Is_Rewrite_Insertion (Node)
              or else not Dump_Original_Only
            then
               Write_Str (", ");
            end if;
         end loop;
      end if;
   end Sprint_Comma_List;

   --------------------------
   -- Sprint_Indented_List --
   --------------------------

   procedure Sprint_Indented_List (List : List_Id) is
   begin
      Indent_Begin;
      Sprint_Node_List (List);
      Indent_End;
   end Sprint_Indented_List;

   ---------------------
   -- Sprint_Left_Opnd --
   ---------------------

   procedure Sprint_Left_Opnd (N : Node_Id) is
      Opnd : constant Node_Id := Left_Opnd (N);

   begin
      if Paren_Count (Opnd) /= 0
        or else Op_Prec (Nkind (Opnd)) >= Op_Prec (Nkind (N))
      then
         Sprint_Node (Opnd);

      else
         Write_Char ('(');
         Sprint_Node (Opnd);
         Write_Char (')');
      end if;
   end Sprint_Left_Opnd;

   -----------------
   -- Sprint_Node --
   -----------------

   procedure Sprint_Node (Node : Node_Id) is
   begin
      if Is_Rewrite_Insertion (Node) then
         if not Dump_Original_Only then

            --  For special cases of nodes that always output <<< >>>
            --  do not duplicate the output at this point.

            if Nkind (Node) = N_Freeze_Entity
              or else Nkind (Node) = N_Implicit_Label_Declaration
            then
               Sprint_Node_Actual (Node);

            --  Normal case where <<< >>> may be required

            else
               Write_Rewrite_Str ("<<<");
               Sprint_Node_Actual (Node);
               Write_Rewrite_Str (">>>");
            end if;
         end if;

      elsif Is_Rewrite_Substitution (Node) then

         --  Case of dump generated only

         if Dump_Generated_Only then
            Sprint_Node_Actual (Node);

         --  Case of dump original only

         elsif Dump_Original_Only then
            Sprint_Node_Actual (Original_Node (Node));

         --  Case of both being dumped

         else
            Sprint_Node_Actual (Original_Node (Node));
            Write_Rewrite_Str ("<<<");
            Sprint_Node_Actual (Node);
            Write_Rewrite_Str (">>>");
         end if;

      else
         Sprint_Node_Actual (Node);
      end if;
   end Sprint_Node;

   ------------------------
   -- Sprint_Node_Actual --
   ------------------------

   procedure Sprint_Node_Actual (Node : Node_Id) is
      Save_Dump_Node : constant Node_Id := Dump_Node;

   begin
      if Node = Empty then
         return;
      end if;

      for J in 1 .. Paren_Count (Node) loop
         Write_Str_With_Col_Check ("(");
      end loop;

      --  Setup current dump node

      Dump_Node := Node;

      if Nkind (Node) in N_Subexpr
        and then Do_Range_Check (Node)
      then
         Write_Str_With_Col_Check ("{");
      end if;

      --  Select print circuit based on node kind

      case Nkind (Node) is

         when N_Abort_Statement =>
            Write_Indent_Str_Sloc ("abort ");
            Sprint_Comma_List (Names (Node));
            Write_Char (';');

         when N_Abortable_Part =>
            Set_Debug_Sloc;
            Write_Str_Sloc ("abort ");
            Sprint_Indented_List (Statements (Node));

         when N_Abstract_Subprogram_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Str_With_Col_Check (" is ");
            Write_Str_Sloc ("abstract;");

         when N_Accept_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));

            if Present (Condition (Node)) then
               Write_Indent_Str ("when ");
               Sprint_Node (Condition (Node));
               Write_Str (" => ");
               Indent_Annull;
            end if;

            Sprint_Node_Sloc (Accept_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Accept_Statement =>
            Write_Indent_Str_Sloc ("accept ");
            Write_Id (Entry_Direct_Name (Node));

            if Present (Entry_Index (Node)) then
               Write_Str_With_Col_Check (" (");
               Sprint_Node (Entry_Index (Node));
               Write_Char (')');
            end if;

            Write_Param_Specs (Node);

            if Present (Handled_Statement_Sequence (Node)) then
               Write_Str_With_Col_Check (" do");
               Sprint_Node (Handled_Statement_Sequence (Node));
               Write_Indent_Str ("end ");
               Write_Id (Entry_Direct_Name (Node));
            end if;

            Write_Char (';');

         when N_Access_Definition =>

            --  Ada 2005 (AI-254)

            if Present (Access_To_Subprogram_Definition (Node)) then
               Sprint_Node (Access_To_Subprogram_Definition (Node));
            else
               --  Ada 2005 (AI-231)

               if Null_Exclusion_Present (Node) then
                  Write_Str ("not null ");
               end if;

               Write_Str_With_Col_Check_Sloc ("access ");

               if All_Present (Node) then
                  Write_Str ("all ");
               elsif Constant_Present (Node) then
                  Write_Str ("constant ");
               end if;

               Sprint_Node (Subtype_Mark (Node));
            end if;

         when N_Access_Function_Definition =>

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str ("not null ");
            end if;

            Write_Str_With_Col_Check_Sloc ("access ");

            if Protected_Present (Node) then
               Write_Str_With_Col_Check ("protected ");
            end if;

            Write_Str_With_Col_Check ("function");
            Write_Param_Specs (Node);
            Write_Str_With_Col_Check (" return ");
            Sprint_Node (Result_Definition (Node));

         when N_Access_Procedure_Definition =>

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str ("not null ");
            end if;

            Write_Str_With_Col_Check_Sloc ("access ");

            if Protected_Present (Node) then
               Write_Str_With_Col_Check ("protected ");
            end if;

            Write_Str_With_Col_Check ("procedure");
            Write_Param_Specs (Node);

         when N_Access_To_Object_Definition =>
            Write_Str_With_Col_Check_Sloc ("access ");

            if All_Present (Node) then
               Write_Str_With_Col_Check ("all ");
            elsif Constant_Present (Node) then
               Write_Str_With_Col_Check ("constant ");
            end if;

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str ("not null ");
            end if;

            Sprint_Node (Subtype_Indication (Node));

         when N_Aggregate =>
            if Null_Record_Present (Node) then
               Write_Str_With_Col_Check_Sloc ("(null record)");

            else
               Write_Str_With_Col_Check_Sloc ("(");

               if Present (Expressions (Node)) then
                  Sprint_Comma_List (Expressions (Node));

                  if Present (Component_Associations (Node)) then
                     Write_Str (", ");
                  end if;
               end if;

               if Present (Component_Associations (Node)) then
                  Indent_Begin;

                  declare
                     Nd : Node_Id;

                  begin
                     Nd := First (Component_Associations (Node));

                     loop
                        Write_Indent;
                        Sprint_Node (Nd);
                        Next (Nd);
                        exit when No (Nd);

                        if not Is_Rewrite_Insertion (Nd)
                          or else not Dump_Original_Only
                        then
                           Write_Str (", ");
                        end if;
                     end loop;
                  end;

                  Indent_End;
               end if;

               Write_Char (')');
            end if;

         when N_Allocator =>
            Write_Str_With_Col_Check_Sloc ("new ");

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str ("not null ");
            end if;

            Sprint_Node (Expression (Node));

            if Present (Storage_Pool (Node)) then
               Write_Str_With_Col_Check ("[storage_pool = ");
               Sprint_Node (Storage_Pool (Node));
               Write_Char (']');
            end if;

         when N_And_Then =>
            Sprint_Left_Opnd (Node);
            Write_Str_Sloc (" and then ");
            Sprint_Right_Opnd (Node);

         when N_At_Clause =>
            Write_Indent_Str_Sloc ("for ");
            Write_Id (Identifier (Node));
            Write_Str_With_Col_Check (" use at ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Assignment_Statement =>
            Write_Indent;
            Sprint_Node (Name (Node));
            Write_Str_Sloc (" := ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Asynchronous_Select =>
            Write_Indent_Str_Sloc ("select");
            Indent_Begin;
            Sprint_Node (Triggering_Alternative (Node));
            Indent_End;

            --  Note: let the printing of Abortable_Part handle outputting
            --  the ABORT keyword, so that the Sloc can be set correctly.

            Write_Indent_Str ("then ");
            Sprint_Node (Abortable_Part (Node));
            Write_Indent_Str ("end select;");

         when N_Attribute_Definition_Clause =>
            Write_Indent_Str_Sloc ("for ");
            Sprint_Node (Name (Node));
            Write_Char (''');
            Write_Name_With_Col_Check (Chars (Node));
            Write_Str_With_Col_Check (" use ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Attribute_Reference =>
            if Is_Procedure_Attribute_Name (Attribute_Name (Node)) then
               Write_Indent;
            end if;

            Sprint_Node (Prefix (Node));
            Write_Char_Sloc (''');
            Write_Name_With_Col_Check (Attribute_Name (Node));
            Sprint_Paren_Comma_List (Expressions (Node));

            if Is_Procedure_Attribute_Name (Attribute_Name (Node)) then
               Write_Char (';');
            end if;

         when N_Block_Statement =>
            Write_Indent;

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Dump_Original_Only)
            then
               Write_Rewrite_Str ("<<<");
               Write_Id (Identifier (Node));
               Write_Str (" : ");
               Write_Rewrite_Str (">>>");
            end if;

            if Present (Declarations (Node)) then
               Write_Str_With_Col_Check_Sloc ("declare");
               Sprint_Indented_List (Declarations (Node));
               Write_Indent;
            end if;

            Write_Str_With_Col_Check_Sloc ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_Str ("end");

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Dump_Original_Only)
            then
               Write_Rewrite_Str ("<<<");
               Write_Char (' ');
               Write_Id (Identifier (Node));
               Write_Rewrite_Str (">>>");
            end if;

            Write_Char (';');

         when N_Case_Statement =>
            Write_Indent_Str_Sloc ("case ");
            Sprint_Node (Expression (Node));
            Write_Str (" is");
            Sprint_Indented_List (Alternatives (Node));
            Write_Indent_Str ("end case;");

         when N_Case_Statement_Alternative =>
            Write_Indent_Str_Sloc ("when ");
            Sprint_Bar_List (Discrete_Choices (Node));
            Write_Str (" => ");
            Sprint_Indented_List (Statements (Node));

         when N_Character_Literal =>
            if Column > 70 then
               Write_Indent_Str ("  ");
            end if;

            Write_Char_Sloc (''');
            Write_Char_Code (UI_To_CC (Char_Literal_Value (Node)));
            Write_Char (''');

         when N_Code_Statement =>
            Write_Indent;
            Set_Debug_Sloc;
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Compilation_Unit =>
            Sprint_Node_List (Context_Items (Node));
            Sprint_Opt_Node_List (Declarations (Aux_Decls_Node (Node)));

            if Private_Present (Node) then
               Write_Indent_Str ("private ");
               Indent_Annull;
            end if;

            Sprint_Node_Sloc (Unit (Node));

            if Present (Actions (Aux_Decls_Node (Node)))
                 or else
               Present (Pragmas_After (Aux_Decls_Node (Node)))
            then
               Write_Indent;
            end if;

            Sprint_Opt_Node_List (Actions (Aux_Decls_Node (Node)));
            Sprint_Opt_Node_List (Pragmas_After (Aux_Decls_Node (Node)));

         when N_Compilation_Unit_Aux =>
            null; -- nothing to do, never used, see above

         when N_Component_Association =>
            Set_Debug_Sloc;
            Sprint_Bar_List (Choices (Node));
            Write_Str (" => ");

            --  Ada 2005 (AI-287): Print the box if present

            if Box_Present (Node) then
               Write_Str_With_Col_Check ("<>");
            else
               Sprint_Node (Expression (Node));
            end if;

         when N_Component_Clause =>
            Write_Indent;
            Sprint_Node (Component_Name (Node));
            Write_Str_Sloc (" at ");
            Sprint_Node (Position (Node));
            Write_Char (' ');
            Write_Str_With_Col_Check ("range ");
            Sprint_Node (First_Bit (Node));
            Write_Str (" .. ");
            Sprint_Node (Last_Bit (Node));
            Write_Char (';');

         when N_Component_Definition =>
            Set_Debug_Sloc;

            --  Ada 2005 (AI-230): Access definition components

            if Present (Access_Definition (Node)) then
               Sprint_Node (Access_Definition (Node));

            elsif Present (Subtype_Indication (Node)) then
               if Aliased_Present (Node) then
                  Write_Str_With_Col_Check ("aliased ");
               end if;

               --  Ada 2005 (AI-231)

               if Null_Exclusion_Present (Node) then
                  Write_Str (" not null ");
               end if;

               Sprint_Node (Subtype_Indication (Node));

            else
               Write_Str (" ??? ");
            end if;

         when N_Component_Declaration =>
            if Write_Indent_Identifiers_Sloc (Node) then
               Write_Str (" : ");
               Sprint_Node (Component_Definition (Node));

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Component_List =>
            if Null_Present (Node) then
               Indent_Begin;
               Write_Indent_Str_Sloc ("null");
               Write_Char (';');
               Indent_End;

            else
               Set_Debug_Sloc;
               Sprint_Indented_List (Component_Items (Node));
               Sprint_Node (Variant_Part (Node));
            end if;

         when N_Conditional_Entry_Call =>
            Write_Indent_Str_Sloc ("select");
            Indent_Begin;
            Sprint_Node (Entry_Call_Alternative (Node));
            Indent_End;
            Write_Indent_Str ("else");
            Sprint_Indented_List (Else_Statements (Node));
            Write_Indent_Str ("end select;");

         when N_Conditional_Expression =>
            declare
               Condition : constant Node_Id := First (Expressions (Node));
               Then_Expr : constant Node_Id := Next (Condition);
               Else_Expr : constant Node_Id := Next (Then_Expr);
            begin
               Write_Str_With_Col_Check_Sloc ("(if ");
               Sprint_Node (Condition);
               Write_Str_With_Col_Check (" then ");
               Sprint_Node (Then_Expr);
               Write_Str_With_Col_Check (" else ");
               Sprint_Node (Else_Expr);
               Write_Char (')');
            end;

         when N_Constrained_Array_Definition =>
            Write_Str_With_Col_Check_Sloc ("array ");
            Sprint_Paren_Comma_List (Discrete_Subtype_Definitions (Node));
            Write_Str (" of ");

            Sprint_Node (Component_Definition (Node));

         when N_Decimal_Fixed_Point_Definition =>
            Write_Str_With_Col_Check_Sloc (" delta ");
            Sprint_Node (Delta_Expression (Node));
            Write_Str_With_Col_Check ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Defining_Character_Literal =>
            Write_Name_With_Col_Check_Sloc (Chars (Node));

         when N_Defining_Identifier =>
            Set_Debug_Sloc;
            Write_Id (Node);

         when N_Defining_Operator_Symbol =>
            Write_Name_With_Col_Check_Sloc (Chars (Node));

         when N_Defining_Program_Unit_Name =>
            Set_Debug_Sloc;
            Sprint_Node (Name (Node));
            Write_Char ('.');
            Write_Id (Defining_Identifier (Node));

         when N_Delay_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));

            if Present (Condition (Node)) then
               Write_Indent;
               Write_Str_With_Col_Check ("when ");
               Sprint_Node (Condition (Node));
               Write_Str (" => ");
               Indent_Annull;
            end if;

            Sprint_Node_Sloc (Delay_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Delay_Relative_Statement =>
            Write_Indent_Str_Sloc ("delay ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Delay_Until_Statement =>
            Write_Indent_Str_Sloc ("delay until ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Delta_Constraint =>
            Write_Str_With_Col_Check_Sloc ("delta ");
            Sprint_Node (Delta_Expression (Node));
            Sprint_Opt_Node (Range_Constraint (Node));

         when N_Derived_Type_Definition =>
            if Abstract_Present (Node) then
               Write_Str_With_Col_Check ("abstract ");
            end if;

            Write_Str_With_Col_Check_Sloc ("new ");

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str_With_Col_Check ("not null ");
            end if;

            Sprint_Node (Subtype_Indication (Node));

            if Present (Interface_List (Node)) then
               Write_Str_With_Col_Check (" and ");
               Sprint_And_List (Interface_List (Node));
               Write_Str_With_Col_Check (" with ");
            end if;

            if Present (Record_Extension_Part (Node)) then
               if No (Interface_List (Node)) then
                  Write_Str_With_Col_Check (" with ");
               end if;

               Sprint_Node (Record_Extension_Part (Node));
            end if;

         when N_Designator =>
            Sprint_Node (Name (Node));
            Write_Char_Sloc ('.');
            Write_Id (Identifier (Node));

         when N_Digits_Constraint =>
            Write_Str_With_Col_Check_Sloc ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Sprint_Opt_Node (Range_Constraint (Node));

         when N_Discriminant_Association =>
            Set_Debug_Sloc;

            if Present (Selector_Names (Node)) then
               Sprint_Bar_List (Selector_Names (Node));
               Write_Str (" => ");
            end if;

            Set_Debug_Sloc;
            Sprint_Node (Expression (Node));

         when N_Discriminant_Specification =>
            Set_Debug_Sloc;

            if Write_Identifiers (Node) then
               Write_Str (" : ");

               if Null_Exclusion_Present (Node) then
                  Write_Str ("not null ");
               end if;

               Sprint_Node (Discriminant_Type (Node));

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;
            else
               Write_Str (", ");
            end if;

         when N_Elsif_Part =>
            Write_Indent_Str_Sloc ("elsif ");
            Sprint_Node (Condition (Node));
            Write_Str_With_Col_Check (" then");
            Sprint_Indented_List (Then_Statements (Node));

         when N_Empty =>
            null;

         when N_Entry_Body =>
            Write_Indent_Str_Sloc ("entry ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Node (Entry_Body_Formal_Part (Node));
            Write_Str_With_Col_Check (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_Str ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_Str ("end ");
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Entry_Body_Formal_Part =>
            if Present (Entry_Index_Specification (Node)) then
               Write_Str_With_Col_Check_Sloc (" (");
               Sprint_Node (Entry_Index_Specification (Node));
               Write_Char (')');
            end if;

            Write_Param_Specs (Node);
            Write_Str_With_Col_Check_Sloc (" when ");
            Sprint_Node (Condition (Node));

         when N_Entry_Call_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));
            Sprint_Node_Sloc (Entry_Call_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Entry_Call_Statement =>
            Write_Indent;
            Sprint_Node_Sloc (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));
            Write_Char (';');

         when N_Entry_Declaration =>
            Write_Indent_Str_Sloc ("entry ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discrete_Subtype_Definition (Node)) then
               Write_Str_With_Col_Check (" (");
               Sprint_Node (Discrete_Subtype_Definition (Node));
               Write_Char (')');
            end if;

            Write_Param_Specs (Node);
            Write_Char (';');

         when N_Entry_Index_Specification =>
            Write_Str_With_Col_Check_Sloc ("for ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" in ");
            Sprint_Node (Discrete_Subtype_Definition (Node));

         when N_Enumeration_Representation_Clause =>
            Write_Indent_Str_Sloc ("for ");
            Write_Id (Identifier (Node));
            Write_Str_With_Col_Check (" use ");
            Sprint_Node (Array_Aggregate (Node));
            Write_Char (';');

         when N_Enumeration_Type_Definition =>
            Set_Debug_Sloc;

            --  Skip attempt to print Literals field if it's not there and
            --  we are in package Standard (case of Character, which is
            --  handled specially (without an explicit literals list).

            if Sloc (Node) > Standard_Location
              or else Present (Literals (Node))
            then
               Sprint_Paren_Comma_List (Literals (Node));
            end if;

         when N_Error =>
            Write_Str_With_Col_Check_Sloc ("<error>");

         when N_Exception_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_Str_With_Col_Check (" : ");

               if Is_Statically_Allocated (Defining_Identifier (Node)) then
                  Write_Str_With_Col_Check ("static ");
               end if;

               Write_Str_Sloc ("exception");

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Exception_Handler =>
            Write_Indent_Str_Sloc ("when ");

            if Present (Choice_Parameter (Node)) then
               Sprint_Node (Choice_Parameter (Node));
               Write_Str (" : ");
            end if;

            Sprint_Bar_List (Exception_Choices (Node));
            Write_Str (" => ");
            Sprint_Indented_List (Statements (Node));

         when N_Exception_Renaming_Declaration =>
            Write_Indent;
            Set_Debug_Sloc;
            Sprint_Node (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" : exception renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Exit_Statement =>
            Write_Indent_Str_Sloc ("exit");
            Sprint_Opt_Node (Name (Node));

            if Present (Condition (Node)) then
               Write_Str_With_Col_Check (" when ");
               Sprint_Node (Condition (Node));
            end if;

            Write_Char (';');

         when N_Expanded_Name =>
            Sprint_Node (Prefix (Node));
            Write_Char_Sloc ('.');
            Sprint_Node (Selector_Name (Node));

         when N_Explicit_Dereference =>
            Sprint_Node (Prefix (Node));
            Write_Char_Sloc ('.');
            Write_Str_Sloc ("all");

         when N_Extended_Return_Statement =>
            Write_Indent_Str_Sloc ("return ");
            Sprint_Node_List (Return_Object_Declarations (Node));

            if Present (Handled_Statement_Sequence (Node)) then
               Write_Str_With_Col_Check (" do");
               Sprint_Node (Handled_Statement_Sequence (Node));
               Write_Indent_Str ("end return;");
            else
               Write_Indent_Str (";");
            end if;

         when N_Extension_Aggregate =>
            Write_Str_With_Col_Check_Sloc ("(");
            Sprint_Node (Ancestor_Part (Node));
            Write_Str_With_Col_Check (" with ");

            if Null_Record_Present (Node) then
               Write_Str_With_Col_Check ("null record");
            else
               if Present (Expressions (Node)) then
                  Sprint_Comma_List (Expressions (Node));

                  if Present (Component_Associations (Node)) then
                     Write_Str (", ");
                  end if;
               end if;

               if Present (Component_Associations (Node)) then
                  Sprint_Comma_List (Component_Associations (Node));
               end if;
            end if;

            Write_Char (')');

         when N_Floating_Point_Definition =>
            Write_Str_With_Col_Check_Sloc ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Formal_Decimal_Fixed_Point_Definition =>
            Write_Str_With_Col_Check_Sloc ("delta <> digits <>");

         when N_Formal_Derived_Type_Definition =>
            Write_Str_With_Col_Check_Sloc ("new ");
            Sprint_Node (Subtype_Mark (Node));

            if Present (Interface_List (Node)) then
               Write_Str_With_Col_Check (" and ");
               Sprint_And_List (Interface_List (Node));
            end if;

            if Private_Present (Node) then
               Write_Str_With_Col_Check (" with private");
            end if;

         when N_Formal_Abstract_Subprogram_Declaration =>
            Write_Indent_Str_Sloc ("with ");
            Sprint_Node (Specification (Node));

            Write_Str_With_Col_Check (" is abstract");

            if Box_Present (Node) then
               Write_Str_With_Col_Check (" <>");
            elsif Present (Default_Name (Node)) then
               Write_Str_With_Col_Check (" ");
               Sprint_Node (Default_Name (Node));
            end if;

            Write_Char (';');

         when N_Formal_Concrete_Subprogram_Declaration =>
            Write_Indent_Str_Sloc ("with ");
            Sprint_Node (Specification (Node));

            if Box_Present (Node) then
               Write_Str_With_Col_Check (" is <>");
            elsif Present (Default_Name (Node)) then
               Write_Str_With_Col_Check (" is ");
               Sprint_Node (Default_Name (Node));
            end if;

            Write_Char (';');

         when N_Formal_Discrete_Type_Definition =>
            Write_Str_With_Col_Check_Sloc ("<>");

         when N_Formal_Floating_Point_Definition =>
            Write_Str_With_Col_Check_Sloc ("digits <>");

         when N_Formal_Modular_Type_Definition =>
            Write_Str_With_Col_Check_Sloc ("mod <>");

         when N_Formal_Object_Declaration =>
            Set_Debug_Sloc;

            if Write_Indent_Identifiers (Node) then
               Write_Str (" : ");

               if In_Present (Node) then
                  Write_Str_With_Col_Check ("in ");
               end if;

               if Out_Present (Node) then
                  Write_Str_With_Col_Check ("out ");
               end if;

               if Present (Subtype_Mark (Node)) then

                  --  Ada 2005 (AI-423): Formal object with null exclusion

                  if Null_Exclusion_Present (Node) then
                     Write_Str ("not null ");
                  end if;

                  Sprint_Node (Subtype_Mark (Node));

               --  Ada 2005 (AI-423): Formal object with access definition

               else
                  pragma Assert (Present (Access_Definition (Node)));

                  Sprint_Node (Access_Definition (Node));
               end if;

               if Present (Default_Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Default_Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            Write_Str_With_Col_Check_Sloc ("delta <>");

         when N_Formal_Package_Declaration =>
            Write_Indent_Str_Sloc ("with package ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" is new ");
            Sprint_Node (Name (Node));
            Write_Str_With_Col_Check (" (<>);");

         when N_Formal_Private_Type_Definition =>
            if Abstract_Present (Node) then
               Write_Str_With_Col_Check ("abstract ");
            end if;

            if Tagged_Present (Node) then
               Write_Str_With_Col_Check ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_Str_With_Col_Check ("limited ");
            end if;

            Write_Str_With_Col_Check_Sloc ("private");

         when N_Formal_Signed_Integer_Type_Definition =>
            Write_Str_With_Col_Check_Sloc ("range <>");

         when N_Formal_Type_Declaration =>
            Write_Indent_Str_Sloc ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Write_Discr_Specs (Node);
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_With_Col_Check ("(<>)");
            end if;

            Write_Str_With_Col_Check (" is ");
            Sprint_Node (Formal_Type_Definition (Node));
            Write_Char (';');

         when N_Free_Statement =>
            Write_Indent_Str_Sloc ("free ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Freeze_Entity =>
            if Dump_Original_Only then
               null;

            elsif Present (Actions (Node)) or else Dump_Freeze_Null then
               Write_Indent;
               Write_Rewrite_Str ("<<<");
               Write_Str_With_Col_Check_Sloc ("freeze ");
               Write_Id (Entity (Node));
               Write_Str (" [");

               if No (Actions (Node)) then
                  Write_Char (']');

               else
                  --  Output freeze actions. We increment Freeze_Indent during
                  --  this output to avoid generating extra blank lines before
                  --  any procedures included in the freeze actions.

                  Freeze_Indent := Freeze_Indent + 1;
                  Sprint_Indented_List (Actions (Node));
                  Freeze_Indent := Freeze_Indent - 1;
                  Write_Indent_Str ("]");
               end if;

               Write_Rewrite_Str (">>>");
            end if;

         when N_Full_Type_Declaration =>
            Write_Indent_Str_Sloc ("type ");
            Sprint_Node (Defining_Identifier (Node));
            Write_Discr_Specs (Node);
            Write_Str_With_Col_Check (" is ");
            Sprint_Node (Type_Definition (Node));
            Write_Char (';');

         when N_Function_Call =>
            Set_Debug_Sloc;
            Note_Implicit_Run_Time_Call (Name (Node));
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));

         when N_Function_Instantiation =>
            Write_Indent_Str_Sloc ("function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));
            Write_Char (';');

         when N_Function_Specification =>
            Write_Str_With_Col_Check_Sloc ("function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Param_Specs (Node);
            Write_Str_With_Col_Check (" return ");

            --  Ada 2005 (AI-231)

            if Nkind (Result_Definition (Node)) /= N_Access_Definition
              and then Null_Exclusion_Present (Node)
            then
               Write_Str (" not null ");
            end if;

            Sprint_Node (Result_Definition (Node));

         when N_Generic_Association =>
            Set_Debug_Sloc;

            if Present (Selector_Name (Node)) then
               Sprint_Node (Selector_Name (Node));
               Write_Str (" => ");
            end if;

            Sprint_Node (Explicit_Generic_Actual_Parameter (Node));

         when N_Generic_Function_Renaming_Declaration =>
            Write_Indent_Str_Sloc ("generic function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Package_Declaration =>
            Extra_Blank_Line;
            Write_Indent_Str_Sloc ("generic ");
            Sprint_Indented_List (Generic_Formal_Declarations (Node));
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Generic_Package_Renaming_Declaration =>
            Write_Indent_Str_Sloc ("generic package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Procedure_Renaming_Declaration =>
            Write_Indent_Str_Sloc ("generic procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Subprogram_Declaration =>
            Extra_Blank_Line;
            Write_Indent_Str_Sloc ("generic ");
            Sprint_Indented_List (Generic_Formal_Declarations (Node));
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Goto_Statement =>
            Write_Indent_Str_Sloc ("goto ");
            Sprint_Node (Name (Node));
            Write_Char (';');

            if Nkind (Next (Node)) = N_Label then
               Write_Indent;
            end if;

         when N_Handled_Sequence_Of_Statements =>
            Set_Debug_Sloc;
            Sprint_Indented_List (Statements (Node));

            if Present (Exception_Handlers (Node)) then
               Write_Indent_Str ("exception");
               Indent_Begin;
               Sprint_Node_List (Exception_Handlers (Node));
               Indent_End;
            end if;

            if Present (At_End_Proc (Node)) then
               Write_Indent_Str ("at end");
               Indent_Begin;
               Write_Indent;
               Sprint_Node (At_End_Proc (Node));
               Write_Char (';');
               Indent_End;
            end if;

         when N_Identifier =>
            Set_Debug_Sloc;
            Write_Id (Node);

         when N_If_Statement =>
            Write_Indent_Str_Sloc ("if ");
            Sprint_Node (Condition (Node));
            Write_Str_With_Col_Check (" then");
            Sprint_Indented_List (Then_Statements (Node));
            Sprint_Opt_Node_List (Elsif_Parts (Node));

            if Present (Else_Statements (Node)) then
               Write_Indent_Str ("else");
               Sprint_Indented_List (Else_Statements (Node));
            end if;

            Write_Indent_Str ("end if;");

         when N_Implicit_Label_Declaration =>
            if not Dump_Original_Only then
               Write_Indent;
               Write_Rewrite_Str ("<<<");
               Set_Debug_Sloc;
               Write_Id (Defining_Identifier (Node));
               Write_Str (" : ");
               Write_Str_With_Col_Check ("label");
               Write_Rewrite_Str (">>>");
            end if;

         when N_In =>
            Sprint_Left_Opnd (Node);
            Write_Str_Sloc (" in ");
            Sprint_Right_Opnd (Node);

         when N_Incomplete_Type_Declaration =>
            Write_Indent_Str_Sloc ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Write_Discr_Specs (Node);
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_With_Col_Check ("(<>)");
            end if;

            Write_Char (';');

         when N_Index_Or_Discriminant_Constraint =>
            Set_Debug_Sloc;
            Sprint_Paren_Comma_List (Constraints (Node));

         when N_Indexed_Component =>
            Sprint_Node_Sloc (Prefix (Node));
            Sprint_Opt_Paren_Comma_List (Expressions (Node));

         when N_Integer_Literal =>
            if Print_In_Hex (Node) then
               Write_Uint_With_Col_Check_Sloc (Intval (Node), Hex);
            else
               Write_Uint_With_Col_Check_Sloc (Intval (Node), Auto);
            end if;

         when N_Iteration_Scheme =>
            if Present (Condition (Node)) then
               Write_Str_With_Col_Check_Sloc ("while ");
               Sprint_Node (Condition (Node));
            else
               Write_Str_With_Col_Check_Sloc ("for ");
               Sprint_Node (Loop_Parameter_Specification (Node));
            end if;

            Write_Char (' ');

         when N_Itype_Reference =>
            Write_Indent_Str_Sloc ("reference ");
            Write_Id (Itype (Node));

         when N_Label =>
            Write_Indent_Str_Sloc ("<<");
            Write_Id (Identifier (Node));
            Write_Str (">>");

         when N_Loop_Parameter_Specification =>
            Set_Debug_Sloc;
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" in ");

            if Reverse_Present (Node) then
               Write_Str_With_Col_Check ("reverse ");
            end if;

            Sprint_Node (Discrete_Subtype_Definition (Node));

         when N_Loop_Statement =>
            Write_Indent;

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Dump_Original_Only)
            then
               Write_Rewrite_Str ("<<<");
               Write_Id (Identifier (Node));
               Write_Str (" : ");
               Write_Rewrite_Str (">>>");
               Sprint_Node (Iteration_Scheme (Node));
               Write_Str_With_Col_Check_Sloc ("loop");
               Sprint_Indented_List (Statements (Node));
               Write_Indent_Str ("end loop ");
               Write_Rewrite_Str ("<<<");
               Write_Id (Identifier (Node));
               Write_Rewrite_Str (">>>");
               Write_Char (';');

            else
               Sprint_Node (Iteration_Scheme (Node));
               Write_Str_With_Col_Check_Sloc ("loop");
               Sprint_Indented_List (Statements (Node));
               Write_Indent_Str ("end loop;");
            end if;

         when N_Mod_Clause =>
            Sprint_Node_List (Pragmas_Before (Node));
            Write_Str_With_Col_Check_Sloc ("at mod ");
            Sprint_Node (Expression (Node));

         when N_Modular_Type_Definition =>
            Write_Str_With_Col_Check_Sloc ("mod ");
            Sprint_Node (Expression (Node));

         when N_Not_In =>
            Sprint_Left_Opnd (Node);
            Write_Str_Sloc (" not in ");
            Sprint_Right_Opnd (Node);

         when N_Null =>
            Write_Str_With_Col_Check_Sloc ("null");

         when N_Null_Statement =>
            if Comes_From_Source (Node)
              or else Dump_Freeze_Null
              or else not Is_List_Member (Node)
              or else (No (Prev (Node)) and then No (Next (Node)))
            then
               Write_Indent_Str_Sloc ("null;");
            end if;

         when N_Number_Declaration =>
            Set_Debug_Sloc;

            if Write_Indent_Identifiers (Node) then
               Write_Str_With_Col_Check (" : constant ");
               Write_Str (" := ");
               Sprint_Node (Expression (Node));
               Write_Char (';');
            end if;

         when N_Object_Declaration =>
            Set_Debug_Sloc;

            if Write_Indent_Identifiers (Node) then
               declare
                  Def_Id : constant Entity_Id := Defining_Identifier (Node);

               begin
                  Write_Str_With_Col_Check (" : ");

                  if Is_Statically_Allocated (Def_Id) then
                     Write_Str_With_Col_Check ("static ");
                  end if;

                  if Aliased_Present (Node) then
                     Write_Str_With_Col_Check ("aliased ");
                  end if;

                  if Constant_Present (Node) then
                     Write_Str_With_Col_Check ("constant ");
                  end if;

                  --  Ada 2005 (AI-231)

                  if Null_Exclusion_Present (Node) then
                     Write_Str_With_Col_Check ("not null ");
                  end if;

                  Sprint_Node (Object_Definition (Node));

                  if Present (Expression (Node)) then
                     Write_Str (" := ");
                     Sprint_Node (Expression (Node));
                  end if;

                  Write_Char (';');

                  --  Handle implicit importation and implicit exportation of
                  --  object declarations:
                  --    $pragma import (Convention_Id, Def_Id, "...");
                  --    $pragma export (Convention_Id, Def_Id, "...");

                  if Is_Internal (Def_Id)
                    and then Present (Interface_Name (Def_Id))
                  then
                     Write_Indent_Str_Sloc ("$pragma ");

                     if Is_Imported (Def_Id) then
                        Write_Str ("import (");

                     else pragma Assert (Is_Exported (Def_Id));
                        Write_Str ("export (");
                     end if;

                     declare
                        Prefix : constant String  := "Convention_";
                        S      : constant String  := Convention (Def_Id)'Img;

                     begin
                        Name_Len := S'Last - Prefix'Last;
                        Name_Buffer (1 .. Name_Len) :=
                          S (Prefix'Last + 1 .. S'Last);
                        Set_Casing (All_Lower_Case);
                        Write_Str (Name_Buffer (1 .. Name_Len));
                     end;

                     Write_Str (", ");
                     Write_Id  (Def_Id);
                     Write_Str (", ");
                     Write_String_Table_Entry
                       (Strval (Interface_Name (Def_Id)));
                     Write_Str (");");
                  end if;
               end;
            end if;

         when N_Object_Renaming_Declaration =>
            Write_Indent;
            Set_Debug_Sloc;
            Sprint_Node (Defining_Identifier (Node));
            Write_Str (" : ");

            --  Ada 2005 (AI-230): Access renamings

            if Present (Access_Definition (Node)) then
               Sprint_Node (Access_Definition (Node));

            elsif Present (Subtype_Mark (Node)) then

               --  Ada 2005 (AI-423): Object renaming with a null exclusion

               if Null_Exclusion_Present (Node) then
                  Write_Str ("not null ");
               end if;

               Sprint_Node (Subtype_Mark (Node));

            else
               Write_Str (" ??? ");
            end if;

            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Op_Abs =>
            Write_Operator (Node, "abs ");
            Sprint_Right_Opnd (Node);

         when N_Op_Add =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " + ");
            Sprint_Right_Opnd (Node);

         when N_Op_And =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " and ");
            Sprint_Right_Opnd (Node);

         when N_Op_Concat =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " & ");
            Sprint_Right_Opnd (Node);

         when N_Op_Divide =>
            Sprint_Left_Opnd (Node);
            Write_Char (' ');
            Process_TFAI_RR_Flags (Node);
            Write_Operator (Node, "/ ");
            Sprint_Right_Opnd (Node);

         when N_Op_Eq =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " = ");
            Sprint_Right_Opnd (Node);

         when N_Op_Expon =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " ** ");
            Sprint_Right_Opnd (Node);

         when N_Op_Ge =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " >= ");
            Sprint_Right_Opnd (Node);

         when N_Op_Gt =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " > ");
            Sprint_Right_Opnd (Node);

         when N_Op_Le =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " <= ");
            Sprint_Right_Opnd (Node);

         when N_Op_Lt =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " < ");
            Sprint_Right_Opnd (Node);

         when N_Op_Minus =>
            Write_Operator (Node, "-");
            Sprint_Right_Opnd (Node);

         when N_Op_Mod =>
            Sprint_Left_Opnd (Node);

            if Treat_Fixed_As_Integer (Node) then
               Write_Str (" #");
            end if;

            Write_Operator (Node, " mod ");
            Sprint_Right_Opnd (Node);

         when N_Op_Multiply =>
            Sprint_Left_Opnd (Node);
            Write_Char (' ');
            Process_TFAI_RR_Flags (Node);
            Write_Operator (Node, "* ");
            Sprint_Right_Opnd (Node);

         when N_Op_Ne =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " /= ");
            Sprint_Right_Opnd (Node);

         when N_Op_Not =>
            Write_Operator (Node, "not ");
            Sprint_Right_Opnd (Node);

         when N_Op_Or =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " or ");
            Sprint_Right_Opnd (Node);

         when N_Op_Plus =>
            Write_Operator (Node, "+");
            Sprint_Right_Opnd (Node);

         when N_Op_Rem =>
            Sprint_Left_Opnd (Node);

            if Treat_Fixed_As_Integer (Node) then
               Write_Str (" #");
            end if;

            Write_Operator (Node, " rem ");
            Sprint_Right_Opnd (Node);

         when N_Op_Shift =>
            Set_Debug_Sloc;
            Write_Id (Node);
            Write_Char ('!');
            Write_Str_With_Col_Check ("(");
            Sprint_Node (Left_Opnd (Node));
            Write_Str (", ");
            Sprint_Node (Right_Opnd (Node));
            Write_Char (')');

         when N_Op_Subtract =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " - ");
            Sprint_Right_Opnd (Node);

         when N_Op_Xor =>
            Sprint_Left_Opnd (Node);
            Write_Operator (Node, " xor ");
            Sprint_Right_Opnd (Node);

         when N_Operator_Symbol =>
            Write_Name_With_Col_Check_Sloc (Chars (Node));

         when N_Ordinary_Fixed_Point_Definition =>
            Write_Str_With_Col_Check_Sloc ("delta ");
            Sprint_Node (Delta_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Or_Else =>
            Sprint_Left_Opnd (Node);
            Write_Str_Sloc (" or else ");
            Sprint_Right_Opnd (Node);

         when N_Others_Choice =>
            if All_Others (Node) then
               Write_Str_With_Col_Check ("all ");
            end if;

            Write_Str_With_Col_Check_Sloc ("others");

         when N_Package_Body =>
            Extra_Blank_Line;
            Write_Indent_Str_Sloc ("package body ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str (" is");
            Sprint_Indented_List (Declarations (Node));

            if Present (Handled_Statement_Sequence (Node)) then
               Write_Indent_Str ("begin");
               Sprint_Node (Handled_Statement_Sequence (Node));
            end if;

            Write_Indent_Str ("end ");
            Sprint_End_Label
              (Handled_Statement_Sequence (Node), Defining_Unit_Name (Node));
            Write_Char (';');

         when N_Package_Body_Stub =>
            Write_Indent_Str_Sloc ("package body ");
            Sprint_Node (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" is separate;");

         when N_Package_Declaration =>
            Extra_Blank_Line;
            Write_Indent;
            Sprint_Node_Sloc (Specification (Node));
            Write_Char (';');

         when N_Package_Instantiation =>
            Extra_Blank_Line;
            Write_Indent_Str_Sloc ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));
            Write_Char (';');

         when N_Package_Renaming_Declaration =>
            Write_Indent_Str_Sloc ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Package_Specification =>
            Write_Str_With_Col_Check_Sloc ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str (" is");
            Sprint_Indented_List (Visible_Declarations (Node));

            if Present (Private_Declarations (Node)) then
               Write_Indent_Str ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_Str ("end ");
            Sprint_Node (Defining_Unit_Name (Node));

         when N_Parameter_Association =>
            Sprint_Node_Sloc (Selector_Name (Node));
            Write_Str (" => ");
            Sprint_Node (Explicit_Actual_Parameter (Node));

         when N_Parameter_Specification =>
            Set_Debug_Sloc;

            if Write_Identifiers (Node) then
               Write_Str (" : ");

               if In_Present (Node) then
                  Write_Str_With_Col_Check ("in ");
               end if;

               if Out_Present (Node) then
                  Write_Str_With_Col_Check ("out ");
               end if;

               --  Ada 2005 (AI-231): Parameter specification may carry null
               --  exclusion. Do not print it now if this is an access formal,
               --  it is emitted when the access definition is displayed.

               if Null_Exclusion_Present (Node)
                 and then Nkind (Parameter_Type (Node))
                   /= N_Access_Definition
               then
                  Write_Str ("not null ");
               end if;

               Sprint_Node (Parameter_Type (Node));

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;
            else
               Write_Str (", ");
            end if;

         when N_Pop_Constraint_Error_Label =>
            Write_Indent_Str ("%pop_constraint_error_label");

         when N_Pop_Program_Error_Label =>
            Write_Indent_Str ("%pop_program_error_label");

         when N_Pop_Storage_Error_Label =>
            Write_Indent_Str ("%pop_storage_error_label");

         when N_Push_Constraint_Error_Label =>
            Write_Indent_Str ("%push_constraint_error_label (");

            if Present (Exception_Label (Node)) then
               Write_Name_With_Col_Check (Chars (Exception_Label (Node)));
            end if;

            Write_Str (")");

         when N_Push_Program_Error_Label =>
            Write_Indent_Str ("%push_program_error_label (");

            if Present (Exception_Label (Node)) then
               Write_Name_With_Col_Check (Chars (Exception_Label (Node)));
            end if;

            Write_Str (")");

         when N_Push_Storage_Error_Label =>
            Write_Indent_Str ("%push_storage_error_label (");

            if Present (Exception_Label (Node)) then
               Write_Name_With_Col_Check (Chars (Exception_Label (Node)));
            end if;

            Write_Str (")");

         when N_Pragma =>
            Write_Indent_Str_Sloc ("pragma ");
            Write_Name_With_Col_Check (Pragma_Name (Node));

            if Present (Pragma_Argument_Associations (Node)) then
               Sprint_Opt_Paren_Comma_List
                 (Pragma_Argument_Associations (Node));
            end if;

            Write_Char (';');

         when N_Pragma_Argument_Association =>
            Set_Debug_Sloc;

            if Chars (Node) /= No_Name then
               Write_Name_With_Col_Check (Chars (Node));
               Write_Str (" => ");
            end if;

            Sprint_Node (Expression (Node));

         when N_Private_Type_Declaration =>
            Write_Indent_Str_Sloc ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Write_Discr_Specs (Node);
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_With_Col_Check ("(<>)");
            end if;

            Write_Str (" is ");

            if Tagged_Present (Node) then
               Write_Str_With_Col_Check ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_Str_With_Col_Check ("limited ");
            end if;

            Write_Str_With_Col_Check ("private;");

         when N_Private_Extension_Declaration =>
            Write_Indent_Str_Sloc ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Write_Discr_Specs (Node);
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_With_Col_Check ("(<>)");
            end if;

            Write_Str_With_Col_Check (" is new ");
            Sprint_Node (Subtype_Indication (Node));

            if Present (Interface_List (Node)) then
               Write_Str_With_Col_Check (" and ");
               Sprint_And_List (Interface_List (Node));
            end if;

            Write_Str_With_Col_Check (" with private;");

         when N_Procedure_Call_Statement =>
            Write_Indent;
            Set_Debug_Sloc;
            Note_Implicit_Run_Time_Call (Name (Node));
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));
            Write_Char (';');

         when N_Procedure_Instantiation =>
            Write_Indent_Str_Sloc ("procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));
            Write_Char (';');

         when N_Procedure_Specification =>
            Write_Str_With_Col_Check_Sloc ("procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Param_Specs (Node);

         when N_Protected_Body =>
            Write_Indent_Str_Sloc ("protected body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_Str ("end ");
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Protected_Body_Stub =>
            Write_Indent_Str_Sloc ("protected body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" is separate;");

         when N_Protected_Definition =>
            Set_Debug_Sloc;
            Sprint_Indented_List (Visible_Declarations (Node));

            if Present (Private_Declarations (Node)) then
               Write_Indent_Str ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_Str ("end ");

         when N_Protected_Type_Declaration =>
            Write_Indent_Str_Sloc ("protected type ");
            Sprint_Node (Defining_Identifier (Node));
            Write_Discr_Specs (Node);

            if Present (Interface_List (Node)) then
               Write_Str (" is new ");
               Sprint_And_List (Interface_List (Node));
               Write_Str (" with ");
            else
               Write_Str (" is");
            end if;

            Sprint_Node (Protected_Definition (Node));
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Qualified_Expression =>
            Sprint_Node (Subtype_Mark (Node));
            Write_Char_Sloc (''');

            --  Print expression, make sure we have at least one level of
            --  parentheses around the expression. For cases of qualified
            --  expressions in the source, this is always the case, but
            --  for generated qualifications, there may be no explicit
            --  parentheses present.

            if Paren_Count (Expression (Node)) /= 0 then
               Sprint_Node (Expression (Node));
            else
               Write_Char ('(');
               Sprint_Node (Expression (Node));
               Write_Char (')');
            end if;

         when N_Raise_Constraint_Error =>

            --  This node can be used either as a subexpression or as a
            --  statement form. The following test is a reasonably reliable
            --  way to distinguish the two cases.

            if Is_List_Member (Node)
              and then Nkind (Parent (Node)) not in N_Subexpr
            then
               Write_Indent;
            end if;

            Write_Str_With_Col_Check_Sloc ("[constraint_error");
            Write_Condition_And_Reason (Node);

         when N_Raise_Program_Error =>

            --  This node can be used either as a subexpression or as a
            --  statement form. The following test is a reasonably reliable
            --  way to distinguish the two cases.

            if Is_List_Member (Node)
              and then Nkind (Parent (Node)) not in N_Subexpr
            then
               Write_Indent;
            end if;

            Write_Str_With_Col_Check_Sloc ("[program_error");
            Write_Condition_And_Reason (Node);

         when N_Raise_Storage_Error =>

            --  This node can be used either as a subexpression or as a
            --  statement form. The following test is a reasonably reliable
            --  way to distinguish the two cases.

            if Is_List_Member (Node)
              and then Nkind (Parent (Node)) not in N_Subexpr
            then
               Write_Indent;
            end if;

            Write_Str_With_Col_Check_Sloc ("[storage_error");
            Write_Condition_And_Reason (Node);

         when N_Raise_Statement =>
            Write_Indent_Str_Sloc ("raise ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Range =>
            Sprint_Node (Low_Bound (Node));
            Write_Str_Sloc (" .. ");
            Sprint_Node (High_Bound (Node));
            Update_Itype (Node);

         when N_Range_Constraint =>
            Write_Str_With_Col_Check_Sloc ("range ");
            Sprint_Node (Range_Expression (Node));

         when N_Real_Literal =>
            Write_Ureal_With_Col_Check_Sloc (Realval (Node));

         when N_Real_Range_Specification =>
            Write_Str_With_Col_Check_Sloc ("range ");
            Sprint_Node (Low_Bound (Node));
            Write_Str (" .. ");
            Sprint_Node (High_Bound (Node));

         when N_Record_Definition =>
            if Abstract_Present (Node) then
               Write_Str_With_Col_Check ("abstract ");
            end if;

            if Tagged_Present (Node) then
               Write_Str_With_Col_Check ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_Str_With_Col_Check ("limited ");
            end if;

            if Null_Present (Node) then
               Write_Str_With_Col_Check_Sloc ("null record");

            else
               Write_Str_With_Col_Check_Sloc ("record");
               Sprint_Node (Component_List (Node));
               Write_Indent_Str ("end record");
            end if;

         when N_Record_Representation_Clause =>
            Write_Indent_Str_Sloc ("for ");
            Sprint_Node (Identifier (Node));
            Write_Str_With_Col_Check (" use record ");

            if Present (Mod_Clause (Node)) then
               Sprint_Node (Mod_Clause (Node));
            end if;

            Sprint_Indented_List (Component_Clauses (Node));
            Write_Indent_Str ("end record;");

         when N_Reference =>
            Sprint_Node (Prefix (Node));
            Write_Str_With_Col_Check_Sloc ("'reference");

         when N_Requeue_Statement =>
            Write_Indent_Str_Sloc ("requeue ");
            Sprint_Node (Name (Node));

            if Abort_Present (Node) then
               Write_Str_With_Col_Check (" with abort");
            end if;

            Write_Char (';');

         when N_Simple_Return_Statement =>
            if Present (Expression (Node)) then
               Write_Indent_Str_Sloc ("return ");
               Sprint_Node (Expression (Node));
               Write_Char (';');
            else
               Write_Indent_Str_Sloc ("return;");
            end if;

         when N_Selective_Accept =>
            Write_Indent_Str_Sloc ("select");

            declare
               Alt_Node : Node_Id;
            begin
               Alt_Node := First (Select_Alternatives (Node));
               loop
                  Indent_Begin;
                  Sprint_Node (Alt_Node);
                  Indent_End;
                  Next (Alt_Node);
                  exit when No (Alt_Node);
                  Write_Indent_Str ("or");
               end loop;
            end;

            if Present (Else_Statements (Node)) then
               Write_Indent_Str ("else");
               Sprint_Indented_List (Else_Statements (Node));
            end if;

            Write_Indent_Str ("end select;");

         when N_Signed_Integer_Type_Definition =>
            Write_Str_With_Col_Check_Sloc ("range ");
            Sprint_Node (Low_Bound (Node));
            Write_Str (" .. ");
            Sprint_Node (High_Bound (Node));

         when N_Single_Protected_Declaration =>
            Write_Indent_Str_Sloc ("protected ");
            Write_Id (Defining_Identifier (Node));
            Write_Str (" is");
            Sprint_Node (Protected_Definition (Node));
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Single_Task_Declaration =>
            Write_Indent_Str_Sloc ("task ");
            Sprint_Node (Defining_Identifier (Node));

            if Present (Task_Definition (Node)) then
               Write_Str (" is");
               Sprint_Node (Task_Definition (Node));
            end if;

            Write_Char (';');

         when N_Selected_Component =>
            Sprint_Node (Prefix (Node));
            Write_Char_Sloc ('.');
            Sprint_Node (Selector_Name (Node));

         when N_Slice =>
            Set_Debug_Sloc;
            Sprint_Node (Prefix (Node));
            Write_Str_With_Col_Check (" (");
            Sprint_Node (Discrete_Range (Node));
            Write_Char (')');

         when N_String_Literal =>
            if String_Length (Strval (Node)) + Column > 75 then
               Write_Indent_Str ("  ");
            end if;

            Set_Debug_Sloc;
            Write_String_Table_Entry (Strval (Node));

         when N_Subprogram_Body =>

            --  Output extra blank line unless we are in freeze actions

            if Freeze_Indent = 0 then
               Extra_Blank_Line;
            end if;

            Write_Indent;
            Sprint_Node_Sloc (Specification (Node));
            Write_Str (" is");

            Sprint_Indented_List (Declarations (Node));
            Write_Indent_Str ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));

            Write_Indent_Str ("end ");

            Sprint_End_Label
              (Handled_Statement_Sequence (Node),
                 Defining_Unit_Name (Specification (Node)));
            Write_Char (';');

            if Is_List_Member (Node)
              and then Present (Next (Node))
              and then Nkind (Next (Node)) /= N_Subprogram_Body
            then
               Write_Indent;
            end if;

         when N_Subprogram_Body_Stub =>
            Write_Indent;
            Sprint_Node_Sloc (Specification (Node));
            Write_Str_With_Col_Check (" is separate;");

         when N_Subprogram_Declaration =>
            Write_Indent;
            Sprint_Node_Sloc (Specification (Node));

            if Nkind (Specification (Node)) = N_Procedure_Specification
              and then Null_Present (Specification (Node))
            then
               Write_Str_With_Col_Check (" is null");
            end if;

            Write_Char (';');

         when N_Subprogram_Info =>
            Sprint_Node (Identifier (Node));
            Write_Str_With_Col_Check_Sloc ("'subprogram_info");

         when N_Subprogram_Renaming_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Str_With_Col_Check_Sloc (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Subtype_Declaration =>
            Write_Indent_Str_Sloc ("subtype ");
            Sprint_Node (Defining_Identifier (Node));
            Write_Str (" is ");

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str ("not null ");
            end if;

            Sprint_Node (Subtype_Indication (Node));
            Write_Char (';');

         when N_Subtype_Indication =>
            Sprint_Node_Sloc (Subtype_Mark (Node));
            Write_Char (' ');
            Sprint_Node (Constraint (Node));

         when N_Subunit =>
            Write_Indent_Str_Sloc ("separate (");
            Sprint_Node (Name (Node));
            Write_Char (')');
            Extra_Blank_Line;
            Sprint_Node (Proper_Body (Node));

         when N_Task_Body =>
            Write_Indent_Str_Sloc ("task body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_Str ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_Str ("end ");
            Sprint_End_Label
              (Handled_Statement_Sequence (Node), Defining_Identifier (Node));
            Write_Char (';');

         when N_Task_Body_Stub =>
            Write_Indent_Str_Sloc ("task body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" is separate;");

         when N_Task_Definition =>
            Set_Debug_Sloc;
            Sprint_Indented_List (Visible_Declarations (Node));

            if Present (Private_Declarations (Node)) then
               Write_Indent_Str ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_Str ("end ");
            Sprint_End_Label (Node, Defining_Identifier (Parent (Node)));

         when N_Task_Type_Declaration =>
            Write_Indent_Str_Sloc ("task type ");
            Sprint_Node (Defining_Identifier (Node));
            Write_Discr_Specs (Node);

            if Present (Interface_List (Node)) then
               Write_Str (" is new ");
               Sprint_And_List (Interface_List (Node));
            end if;

            if Present (Task_Definition (Node)) then
               if No (Interface_List (Node)) then
                  Write_Str (" is");
               else
                  Write_Str (" with ");
               end if;

               Sprint_Node (Task_Definition (Node));
            end if;

            Write_Char (';');

         when N_Terminate_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));

            Write_Indent;

            if Present (Condition (Node)) then
               Write_Str_With_Col_Check ("when ");
               Sprint_Node (Condition (Node));
               Write_Str (" => ");
            end if;

            Write_Str_With_Col_Check_Sloc ("terminate;");
            Sprint_Node_List (Pragmas_After (Node));

         when N_Timed_Entry_Call =>
            Write_Indent_Str_Sloc ("select");
            Indent_Begin;
            Sprint_Node (Entry_Call_Alternative (Node));
            Indent_End;
            Write_Indent_Str ("or");
            Indent_Begin;
            Sprint_Node (Delay_Alternative (Node));
            Indent_End;
            Write_Indent_Str ("end select;");

         when N_Triggering_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));
            Sprint_Node_Sloc (Triggering_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Type_Conversion =>
            Set_Debug_Sloc;
            Sprint_Node (Subtype_Mark (Node));
            Col_Check (4);

            if Conversion_OK (Node) then
               Write_Char ('?');
            end if;

            if Float_Truncate (Node) then
               Write_Char ('^');
            end if;

            if Rounded_Result (Node) then
               Write_Char ('@');
            end if;

            Write_Char ('(');
            Sprint_Node (Expression (Node));
            Write_Char (')');

         when N_Unchecked_Expression =>
            Col_Check (10);
            Write_Str ("`(");
            Sprint_Node_Sloc (Expression (Node));
            Write_Char (')');

         when N_Unchecked_Type_Conversion =>
            Sprint_Node (Subtype_Mark (Node));
            Write_Char ('!');
            Write_Str_With_Col_Check ("(");
            Sprint_Node_Sloc (Expression (Node));
            Write_Char (')');

         when N_Unconstrained_Array_Definition =>
            Write_Str_With_Col_Check_Sloc ("array (");

            declare
               Node1 : Node_Id;
            begin
               Node1 := First (Subtype_Marks (Node));
               loop
                  Sprint_Node (Node1);
                  Write_Str_With_Col_Check (" range <>");
                  Next (Node1);
                  exit when Node1 = Empty;
                  Write_Str (", ");
               end loop;
            end;

            Write_Str (") of ");
            Sprint_Node (Component_Definition (Node));

         when N_Unused_At_Start | N_Unused_At_End =>
            Write_Indent_Str ("***** Error, unused node encountered *****");
            Write_Eol;

         when N_Use_Package_Clause =>
            Write_Indent_Str_Sloc ("use ");
            Sprint_Comma_List (Names (Node));
            Write_Char (';');

         when N_Use_Type_Clause =>
            Write_Indent_Str_Sloc ("use type ");
            Sprint_Comma_List (Subtype_Marks (Node));
            Write_Char (';');

         when N_Validate_Unchecked_Conversion =>
            Write_Indent_Str_Sloc ("validate unchecked_conversion (");
            Sprint_Node (Source_Type (Node));
            Write_Str (", ");
            Sprint_Node (Target_Type (Node));
            Write_Str (");");

         when N_Variant =>
            Write_Indent_Str_Sloc ("when ");
            Sprint_Bar_List (Discrete_Choices (Node));
            Write_Str (" => ");
            Sprint_Node (Component_List (Node));

         when N_Variant_Part =>
            Indent_Begin;
            Write_Indent_Str_Sloc ("case ");
            Sprint_Node (Name (Node));
            Write_Str (" is ");
            Sprint_Indented_List (Variants (Node));
            Write_Indent_Str ("end case");
            Indent_End;

         when N_With_Clause =>

            --  Special test, if we are dumping the original tree only,
            --  then we want to eliminate the bogus with clauses that
            --  correspond to the non-existent children of Text_IO.

            if Dump_Original_Only
              and then Is_Text_IO_Kludge_Unit (Name (Node))
            then
               null;

            --  Normal case, output the with clause

            else
               if First_Name (Node) or else not Dump_Original_Only then

                  --  Ada 2005 (AI-50217): Print limited with_clauses

                  if Private_Present (Node) and Limited_Present (Node) then
                     Write_Indent_Str ("limited private with ");

                  elsif Private_Present (Node) then
                     Write_Indent_Str ("private with ");

                  elsif Limited_Present (Node) then
                     Write_Indent_Str ("limited with ");

                  else
                     Write_Indent_Str ("with ");
                  end if;

               else
                  Write_Str (", ");
               end if;

               Sprint_Node_Sloc (Name (Node));

               if Last_Name (Node) or else not Dump_Original_Only then
                  Write_Char (';');
               end if;
            end if;

      end case;

      if Nkind (Node) in N_Subexpr
        and then Do_Range_Check (Node)
      then
         Write_Str ("}");
      end if;

      for J in 1 .. Paren_Count (Node) loop
         Write_Char (')');
      end loop;

      Dump_Node := Save_Dump_Node;
   end Sprint_Node_Actual;

   ----------------------
   -- Sprint_Node_List --
   ----------------------

   procedure Sprint_Node_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Sprint_Node (Node);
            Next (Node);
            exit when Node = Empty;
         end loop;
      end if;
   end Sprint_Node_List;

   ----------------------
   -- Sprint_Node_Sloc --
   ----------------------

   procedure Sprint_Node_Sloc (Node : Node_Id) is
   begin
      Sprint_Node (Node);

      if Debug_Generated_Code and then Present (Dump_Node) then
         Set_Sloc (Dump_Node, Sloc (Node));
         Dump_Node := Empty;
      end if;
   end Sprint_Node_Sloc;

   ---------------------
   -- Sprint_Opt_Node --
   ---------------------

   procedure Sprint_Opt_Node (Node : Node_Id) is
   begin
      if Present (Node) then
         Write_Char (' ');
         Sprint_Node (Node);
      end if;
   end Sprint_Opt_Node;

   --------------------------
   -- Sprint_Opt_Node_List --
   --------------------------

   procedure Sprint_Opt_Node_List (List : List_Id) is
   begin
      if Present (List) then
         Sprint_Node_List (List);
      end if;
   end Sprint_Opt_Node_List;

   ---------------------------------
   -- Sprint_Opt_Paren_Comma_List --
   ---------------------------------

   procedure Sprint_Opt_Paren_Comma_List (List : List_Id) is
   begin
      if Is_Non_Empty_List (List) then
         Write_Char (' ');
         Sprint_Paren_Comma_List (List);
      end if;
   end Sprint_Opt_Paren_Comma_List;

   -----------------------------
   -- Sprint_Paren_Comma_List --
   -----------------------------

   procedure Sprint_Paren_Comma_List (List : List_Id) is
      N           : Node_Id;
      Node_Exists : Boolean := False;

   begin

      if Is_Non_Empty_List (List) then

         if Dump_Original_Only then
            N := First (List);
            while Present (N) loop
               if not Is_Rewrite_Insertion (N) then
                  Node_Exists := True;
                  exit;
               end if;

               Next (N);
            end loop;

            if not Node_Exists then
               return;
            end if;
         end if;

         Write_Str_With_Col_Check ("(");
         Sprint_Comma_List (List);
         Write_Char (')');
      end if;
   end Sprint_Paren_Comma_List;

   ----------------------
   -- Sprint_Right_Opnd --
   ----------------------

   procedure Sprint_Right_Opnd (N : Node_Id) is
      Opnd : constant Node_Id := Right_Opnd (N);

   begin
      if Paren_Count (Opnd) /= 0
        or else Op_Prec (Nkind (Opnd)) > Op_Prec (Nkind (N))
      then
         Sprint_Node (Opnd);

      else
         Write_Char ('(');
         Sprint_Node (Opnd);
         Write_Char (')');
      end if;
   end Sprint_Right_Opnd;

   ------------------
   -- Update_Itype --
   ------------------

   procedure Update_Itype (Node : Node_Id) is
   begin
      if Present (Etype (Node))
        and then Is_Itype (Etype (Node))
        and then Debug_Generated_Code
      then
         Set_Sloc (Etype (Node), Sloc (Node));
      end if;
   end Update_Itype;

   ---------------------
   -- Write_Char_Sloc --
   ---------------------

   procedure Write_Char_Sloc (C : Character) is
   begin
      if Debug_Generated_Code and then C /= ' ' then
         Set_Debug_Sloc;
      end if;

      Write_Char (C);
   end Write_Char_Sloc;

   --------------------------------
   -- Write_Condition_And_Reason --
   --------------------------------

   procedure Write_Condition_And_Reason (Node : Node_Id) is
      Cond  : constant Node_Id := Condition (Node);
      Image : constant String  := RT_Exception_Code'Image
                                    (RT_Exception_Code'Val
                                       (UI_To_Int (Reason (Node))));

   begin
      if Present (Cond) then

         --  If condition is a single entity, or NOT with a single entity,
         --  output all on one line, since it will likely fit just fine.

         if Is_Entity_Name (Cond)
           or else (Nkind (Cond) = N_Op_Not
                     and then Is_Entity_Name (Right_Opnd (Cond)))
         then
            Write_Str_With_Col_Check (" when ");
            Sprint_Node (Cond);
            Write_Char (' ');

            --  Otherwise for more complex condition, multiple lines

         else
            Write_Str_With_Col_Check (" when");
            Indent := Indent + 2;
            Write_Indent;
            Sprint_Node (Cond);
            Write_Indent;
            Indent := Indent - 2;
         end if;

      --  If no condition, just need a space (all on one line)

      else
         Write_Char (' ');
      end if;

      --  Write the reason

      Write_Char ('"');

      for J in 4 .. Image'Last loop
         if Image (J) = '_' then
            Write_Char (' ');
         else
            Write_Char (Fold_Lower (Image (J)));
         end if;
      end loop;

      Write_Str ("""]");
   end Write_Condition_And_Reason;

   --------------------------------
   -- Write_Corresponding_Source --
   --------------------------------

   procedure Write_Corresponding_Source (S : String) is
      Loc : Source_Ptr;
      Src : Source_Buffer_Ptr;

   begin
      --  Ignore if not in dump source text mode, or if in freeze actions

      if Dump_Source_Text and then Freeze_Indent = 0 then

         --  Ignore null string

         if S = "" then
            return;
         end if;

         --  Ignore space or semicolon at end of given string

         if S (S'Last) = ' ' or else S (S'Last) = ';' then
            Write_Corresponding_Source (S (S'First .. S'Last - 1));
            return;
         end if;

         --  Loop to look at next lines not yet printed in source file

         for L in
           Last_Line_Printed + 1 .. Last_Source_Line (Current_Source_File)
         loop
            Src := Source_Text (Current_Source_File);
            Loc := Line_Start (L, Current_Source_File);

            --  If comment, keep looking

            if Src (Loc .. Loc + 1) = "--" then
               null;

            --  Search to first non-blank

            else
               while Src (Loc) not in Line_Terminator loop

                  --  Non-blank found

                  if Src (Loc) /= ' ' and then Src (Loc) /= ASCII.HT then

                     --  Loop through characters in string to see if we match

                     for J in S'Range loop

                        --  If mismatch, then not the case we are looking for

                        if Src (Loc) /= S (J) then
                           return;
                        end if;

                        Loc := Loc + 1;
                     end loop;

                     --  If we fall through, string matched, if white space or
                     --  semicolon after the matched string, this is the case
                     --  we are looking for.

                     if Src (Loc) in Line_Terminator
                       or else Src (Loc) = ' '
                       or else Src (Loc) = ASCII.HT
                       or else Src (Loc) = ';'
                     then
                        --  So output source lines up to and including this one

                        Write_Source_Lines (L);
                        return;
                     end if;
                  end if;

                  Loc := Loc + 1;
               end loop;
            end if;

         --  Line was all blanks, or a comment line, keep looking

         end loop;
      end if;
   end Write_Corresponding_Source;

   -----------------------
   -- Write_Discr_Specs --
   -----------------------

   procedure Write_Discr_Specs (N : Node_Id) is
      Specs : List_Id;
      Spec  : Node_Id;

   begin
      Specs := Discriminant_Specifications (N);

      if Present (Specs) then
         Write_Str_With_Col_Check (" (");
         Spec := First (Specs);

         loop
            Sprint_Node (Spec);
            Next (Spec);
            exit when Spec = Empty;

            --  Add semicolon, unless we are printing original tree and the
            --  next specification is part of a list (but not the first
            --  element of that list)

            if not Dump_Original_Only or else not Prev_Ids (Spec) then
               Write_Str ("; ");
            end if;
         end loop;

         Write_Char (')');
      end if;
   end Write_Discr_Specs;

   -----------------
   -- Write_Ekind --
   -----------------

   procedure Write_Ekind (E : Entity_Id) is
      S : constant String := Entity_Kind'Image (Ekind (E));

   begin
      Name_Len := S'Length;
      Name_Buffer (1 .. Name_Len) := S;
      Set_Casing (Mixed_Case);
      Write_Str_With_Col_Check (Name_Buffer (1 .. Name_Len));
   end Write_Ekind;

   --------------
   -- Write_Id --
   --------------

   procedure Write_Id (N : Node_Id) is
   begin
      --  Deal with outputting Itype

      --  Note: if we are printing the full tree with -gnatds, then we may
      --  end up picking up the Associated_Node link from a generic template
      --  here which overlaps the Entity field, but as documented, Write_Itype
      --  is defended against junk calls.

      if Nkind (N) in N_Entity then
         Write_Itype (N);
      elsif Nkind (N) in N_Has_Entity then
         Write_Itype (Entity (N));
      end if;

      --  Case of a defining identifier

      if Nkind (N) = N_Defining_Identifier then

         --  If defining identifier has an interface name (and no
         --  address clause), then we output the interface name.

         if (Is_Imported (N) or else Is_Exported (N))
           and then Present (Interface_Name (N))
           and then No (Address_Clause (N))
         then
            String_To_Name_Buffer (Strval (Interface_Name (N)));
            Write_Str_With_Col_Check (Name_Buffer (1 .. Name_Len));

         --  If no interface name (or inactive because there was
         --  an address clause), then just output the Chars name.

         else
            Write_Name_With_Col_Check (Chars (N));
         end if;

      --  Case of selector of an expanded name where the expanded name
      --  has an associated entity, output this entity.

      elsif Nkind (Parent (N)) = N_Expanded_Name
        and then Selector_Name (Parent (N)) = N
        and then Present (Entity (Parent (N)))
      then
         Write_Id (Entity (Parent (N)));

      --  For any other node with an associated entity, output it

      elsif Nkind (N) in N_Has_Entity
        and then Present (Entity_Or_Associated_Node (N))
        and then Nkind (Entity_Or_Associated_Node (N)) in N_Entity
      then
         Write_Id (Entity (N));

      --  All other cases, we just print the Chars field

      else
         Write_Name_With_Col_Check (Chars (N));
      end if;
   end Write_Id;

   -----------------------
   -- Write_Identifiers --
   -----------------------

   function Write_Identifiers (Node : Node_Id) return Boolean is
   begin
      Sprint_Node (Defining_Identifier (Node));
      Update_Itype (Defining_Identifier (Node));

      --  The remainder of the declaration must be printed unless we are
      --  printing the original tree and this is not the last identifier

      return
         not Dump_Original_Only or else not More_Ids (Node);

   end Write_Identifiers;

   ------------------------
   -- Write_Implicit_Def --
   ------------------------

   procedure Write_Implicit_Def (E : Entity_Id) is
      Ind : Node_Id;

   begin
      case Ekind (E) is
         when E_Array_Subtype =>
            Write_Str_With_Col_Check ("subtype ");
            Write_Id (E);
            Write_Str_With_Col_Check (" is ");
            Write_Id (Base_Type (E));
            Write_Str_With_Col_Check (" (");

            Ind := First_Index (E);
            while Present (Ind) loop
               Sprint_Node (Ind);
               Next_Index (Ind);

               if Present (Ind) then
                  Write_Str (", ");
               end if;
            end loop;

            Write_Str (");");

         when E_Signed_Integer_Subtype | E_Enumeration_Subtype =>
            Write_Str_With_Col_Check ("subtype ");
            Write_Id (E);
            Write_Str (" is ");
            Write_Id (Etype (E));
            Write_Str_With_Col_Check (" range ");
            Sprint_Node (Scalar_Range (E));
            Write_Str (";");

         when others =>
            Write_Str_With_Col_Check ("type ");
            Write_Id (E);
            Write_Str_With_Col_Check (" is <");
            Write_Ekind (E);
            Write_Str (">;");
      end case;

   end Write_Implicit_Def;

   ------------------
   -- Write_Indent --
   ------------------

   procedure Write_Indent is
      Loc : constant Source_Ptr := Sloc (Dump_Node);

   begin
      if Indent_Annull_Flag then
         Indent_Annull_Flag := False;
      else
         --  Deal with Dump_Source_Text output. Note that we ignore implicit
         --  label declarations, since they typically have the sloc of the
         --  corresponding label, which really messes up the -gnatL output.

         if Dump_Source_Text
           and then Loc > No_Location
           and then Nkind (Dump_Node) /= N_Implicit_Label_Declaration
         then
            if Get_Source_File_Index (Loc) = Current_Source_File then
               Write_Source_Lines
                 (Get_Physical_Line_Number (Sloc (Dump_Node)));
            end if;
         end if;

         Write_Eol;

         for J in 1 .. Indent loop
            Write_Char (' ');
         end loop;
      end if;
   end Write_Indent;

   ------------------------------
   -- Write_Indent_Identifiers --
   ------------------------------

   function Write_Indent_Identifiers (Node : Node_Id) return Boolean is
   begin
      --  We need to start a new line for every node, except in the case
      --  where we are printing the original tree and this is not the first
      --  defining identifier in the list.

      if not Dump_Original_Only or else not Prev_Ids (Node) then
         Write_Indent;

      --  If printing original tree and this is not the first defining
      --  identifier in the list, then the previous call to this procedure
      --  printed only the name, and we add a comma to separate the names.

      else
         Write_Str (", ");
      end if;

      Sprint_Node (Defining_Identifier (Node));

      --  The remainder of the declaration must be printed unless we are
      --  printing the original tree and this is not the last identifier

      return
         not Dump_Original_Only or else not More_Ids (Node);
   end Write_Indent_Identifiers;

   -----------------------------------
   -- Write_Indent_Identifiers_Sloc --
   -----------------------------------

   function Write_Indent_Identifiers_Sloc (Node : Node_Id) return Boolean is
   begin
      --  We need to start a new line for every node, except in the case
      --  where we are printing the original tree and this is not the first
      --  defining identifier in the list.

      if not Dump_Original_Only or else not Prev_Ids (Node) then
         Write_Indent;

      --  If printing original tree and this is not the first defining
      --  identifier in the list, then the previous call to this procedure
      --  printed only the name, and we add a comma to separate the names.

      else
         Write_Str (", ");
      end if;

      Set_Debug_Sloc;
      Sprint_Node (Defining_Identifier (Node));

      --  The remainder of the declaration must be printed unless we are
      --  printing the original tree and this is not the last identifier

      return not Dump_Original_Only or else not More_Ids (Node);
   end Write_Indent_Identifiers_Sloc;

   ----------------------
   -- Write_Indent_Str --
   ----------------------

   procedure Write_Indent_Str (S : String) is
   begin
      Write_Corresponding_Source (S);
      Write_Indent;
      Write_Str (S);
   end Write_Indent_Str;

   ---------------------------
   -- Write_Indent_Str_Sloc --
   ---------------------------

   procedure Write_Indent_Str_Sloc (S : String) is
   begin
      Write_Corresponding_Source (S);
      Write_Indent;
      Write_Str_Sloc (S);
   end Write_Indent_Str_Sloc;

   -----------------
   -- Write_Itype --
   -----------------

   procedure Write_Itype (Typ : Entity_Id) is

      procedure Write_Header (T : Boolean := True);
      --  Write type if T is True, subtype if T is false

      ------------------
      -- Write_Header --
      ------------------

      procedure Write_Header (T : Boolean := True) is
      begin
         if T then
            Write_Str ("[type ");
         else
            Write_Str ("[subtype ");
         end if;

         Write_Name_With_Col_Check (Chars (Typ));
         Write_Str (" is ");
      end Write_Header;

   --  Start of processing for Write_Itype

   begin
      if Nkind (Typ) in N_Entity
        and then Is_Itype (Typ)
        and then not Itype_Printed (Typ)
      then
         --  Itype to be printed

         declare
            B : constant Node_Id := Etype (Typ);
            X : Node_Id;
            P : constant Node_Id := Parent (Typ);

            S : constant Saved_Output_Buffer := Save_Output_Buffer;
            --  Save current output buffer

            Old_Sloc : Source_Ptr;
            --  Save sloc of related node, so it is not modified when
            --  printing with -gnatD.

         begin
            --  Write indentation at start of line

            for J in 1 .. Indent loop
               Write_Char (' ');
            end loop;

            --  If we have a constructed declaration for the itype, print it

            if Present (P)
              and then Nkind (P) in N_Declaration
              and then Defining_Entity (P) = Typ
            then
               --  We must set Itype_Printed true before the recursive call to
               --  print the node, otherwise we get an infinite recursion!

               Set_Itype_Printed (Typ, True);

               --  Write the declaration enclosed in [], avoiding new line
               --  at start of declaration, and semicolon at end.

               --  Note: The itype may be imported from another unit, in which
               --  case we do not want to modify the Sloc of the declaration.
               --  Otherwise the itype may appear to be in the current unit,
               --  and the back-end will reject a reference out of scope.

               Write_Char ('[');
               Indent_Annull_Flag := True;
               Old_Sloc := Sloc (P);
               Sprint_Node (P);
               Set_Sloc (P, Old_Sloc);
               Write_Erase_Char (';');

            --  If no constructed declaration, then we have to concoct the
            --  source corresponding to the type entity that we have at hand.

            else
               case Ekind (Typ) is

                  --  Access types and subtypes

                  when Access_Kind =>
                     Write_Header (Ekind (Typ) = E_Access_Type);
                     Write_Str ("access ");

                     if Is_Access_Constant (Typ) then
                        Write_Str ("constant ");
                     elsif Can_Never_Be_Null (Typ) then
                        Write_Str ("not null ");
                     end if;

                     Write_Id (Directly_Designated_Type (Typ));

                     --  Array types and string types

                  when E_Array_Type | E_String_Type =>
                     Write_Header;
                     Write_Str ("array (");

                     X := First_Index (Typ);
                     loop
                        Sprint_Node (X);

                        if not Is_Constrained (Typ) then
                           Write_Str (" range <>");
                        end if;

                        Next_Index (X);
                        exit when No (X);
                        Write_Str (", ");
                     end loop;

                     Write_Str (") of ");
                     X := Component_Type (Typ);

                     --  Preserve sloc of component type, which is defined
                     --  elsewhere than the itype (see comment above).

                     Old_Sloc := Sloc (X);
                     Sprint_Node (X);
                     Set_Sloc (X, Old_Sloc);

                     --  Array subtypes and string subtypes

                  when E_Array_Subtype | E_String_Subtype =>
                     Write_Header (False);
                     Write_Id (Etype (Typ));
                     Write_Str (" (");

                     X := First_Index (Typ);
                     loop
                        Sprint_Node (X);
                        Next_Index (X);
                        exit when No (X);
                        Write_Str (", ");
                     end loop;

                     Write_Char (')');

                     --  Signed integer types, and modular integer subtypes

                  when E_Signed_Integer_Type     |
                       E_Signed_Integer_Subtype  |
                       E_Modular_Integer_Subtype =>

                     Write_Header (Ekind (Typ) = E_Signed_Integer_Type);

                     if Ekind (Typ) = E_Signed_Integer_Type then
                        Write_Str ("new ");
                     end if;

                     Write_Id (B);

                     --  Print bounds if different from base type

                     declare
                        L  : constant Node_Id := Type_Low_Bound (Typ);
                        H  : constant Node_Id := Type_High_Bound (Typ);
                        LE : Node_Id;
                        HE : Node_Id;

                     begin
                        --  B can either be a scalar type, in which case the
                        --  declaration of Typ may constrain it with different
                        --  bounds, or a private type, in which case we know
                        --  that the declaration of Typ cannot have a scalar
                        --  constraint.

                        if Is_Scalar_Type (B) then
                           LE := Type_Low_Bound (B);
                           HE := Type_High_Bound (B);
                        else
                           LE := Empty;
                           HE := Empty;
                        end if;

                        if No (LE)
                          or else (True
                            and then Nkind (L) = N_Integer_Literal
                            and then Nkind (H) = N_Integer_Literal
                            and then Nkind (LE) = N_Integer_Literal
                            and then Nkind (HE) = N_Integer_Literal
                            and then UI_Eq (Intval (L), Intval (LE))
                            and then UI_Eq (Intval (H), Intval (HE)))
                        then
                           null;

                        else
                           Write_Str (" range ");
                           Sprint_Node (Type_Low_Bound (Typ));
                           Write_Str (" .. ");
                           Sprint_Node (Type_High_Bound (Typ));
                        end if;
                     end;

                     --  Modular integer types

                  when E_Modular_Integer_Type =>
                     Write_Header;
                     Write_Str (" mod ");
                     Write_Uint_With_Col_Check (Modulus (Typ), Auto);

                     --  Floating point types and subtypes

                  when E_Floating_Point_Type    |
                       E_Floating_Point_Subtype =>

                     Write_Header (Ekind (Typ) = E_Floating_Point_Type);

                     if Ekind (Typ) = E_Floating_Point_Type then
                        Write_Str ("new ");
                     end if;

                     Write_Id (Etype (Typ));

                     if Digits_Value (Typ) /= Digits_Value (Etype (Typ)) then
                        Write_Str (" digits ");
                        Write_Uint_With_Col_Check
                          (Digits_Value (Typ), Decimal);
                     end if;

                     --  Print bounds if not different from base type

                     declare
                        L  : constant Node_Id := Type_Low_Bound (Typ);
                        H  : constant Node_Id := Type_High_Bound (Typ);
                        LE : constant Node_Id := Type_Low_Bound (B);
                        HE : constant Node_Id := Type_High_Bound (B);

                     begin
                        if Nkind (L) = N_Real_Literal
                          and then Nkind (H) = N_Real_Literal
                          and then Nkind (LE) = N_Real_Literal
                          and then Nkind (HE) = N_Real_Literal
                          and then UR_Eq (Realval (L), Realval (LE))
                          and then UR_Eq (Realval (H), Realval (HE))
                        then
                           null;

                        else
                           Write_Str (" range ");
                           Sprint_Node (Type_Low_Bound (Typ));
                           Write_Str (" .. ");
                           Sprint_Node (Type_High_Bound (Typ));
                        end if;
                     end;

                  --  Record subtypes

                  when E_Record_Subtype =>
                     Write_Header (False);
                     Write_Str ("record");
                     Indent_Begin;

                     declare
                        C : Entity_Id;
                     begin
                        C := First_Entity (Typ);
                        while Present (C) loop
                           Write_Indent;
                           Write_Id (C);
                           Write_Str (" : ");
                           Write_Id (Etype (C));
                           Next_Entity (C);
                        end loop;
                     end;

                     Indent_End;
                     Write_Indent_Str (" end record");

                  --  Class-Wide types

                  when E_Class_Wide_Type    |
                       E_Class_Wide_Subtype =>
                     Write_Header;
                     Write_Name_With_Col_Check (Chars (Etype (Typ)));
                     Write_Str ("'Class");

                  --  Subprogram types

                  when E_Subprogram_Type =>
                     Write_Header;

                     if Etype (Typ) = Standard_Void_Type then
                        Write_Str ("procedure");
                     else
                        Write_Str ("function");
                     end if;

                     if Present (First_Entity (Typ)) then
                        Write_Str (" (");

                        declare
                           Param : Entity_Id;

                        begin
                           Param := First_Entity (Typ);
                           loop
                              Write_Id (Param);
                              Write_Str (" : ");

                              if Ekind (Param) = E_In_Out_Parameter then
                                 Write_Str ("in out ");
                              elsif Ekind (Param) = E_Out_Parameter then
                                 Write_Str ("out ");
                              end if;

                              Write_Id (Etype (Param));
                              Next_Entity (Param);
                              exit when No (Param);
                              Write_Str (", ");
                           end loop;

                           Write_Char (')');
                        end;
                     end if;

                     if Etype (Typ) /= Standard_Void_Type then
                        Write_Str (" return ");
                        Write_Id (Etype (Typ));
                     end if;

                  when E_String_Literal_Subtype =>
                     declare
                        LB  : constant Uint :=
                                Intval (String_Literal_Low_Bound (Typ));
                        Len : constant Uint :=
                                String_Literal_Length (Typ);
                     begin
                        Write_Str ("String (");
                        Write_Int (UI_To_Int (LB));
                        Write_Str (" .. ");
                        Write_Int (UI_To_Int (LB + Len) - 1);
                        Write_Str (");");
                     end;

                  --  For all other Itypes, print ??? (fill in later)

                  when others =>
                     Write_Header (True);
                     Write_Str ("???");

               end case;
            end if;

            --  Add terminating bracket and restore output buffer

            Write_Char (']');
            Write_Eol;
            Restore_Output_Buffer (S);
         end;

         Set_Itype_Printed (Typ);
      end if;
   end Write_Itype;

   -------------------------------
   -- Write_Name_With_Col_Check --
   -------------------------------

   procedure Write_Name_With_Col_Check (N : Name_Id) is
      J : Natural;
      K : Natural;
      L : Natural;

   begin
      Get_Name_String (N);

      --  Deal with -gnatdI which replaces any sequence Cnnnb where C is an
      --  upper case letter, nnn is one or more digits and b is a lower case
      --  letter by C...b, so that listings do not depend on serial numbers.

      if Debug_Flag_II then
         J := 1;
         while J < Name_Len - 1 loop
            if Name_Buffer (J) in 'A' .. 'Z'
              and then Name_Buffer (J + 1) in '0' .. '9'
            then
               K := J + 1;
               while K < Name_Len loop
                  exit when Name_Buffer (K) not in '0' .. '9';
                  K := K + 1;
               end loop;

               if Name_Buffer (K) in 'a' .. 'z' then
                  L := Name_Len - K + 1;

                  Name_Buffer (J + 4 .. J + L + 3) :=
                    Name_Buffer (K .. Name_Len);
                  Name_Buffer (J + 1 .. J + 3) := "...";
                  Name_Len := J + L + 3;
                  J := J + 5;

               else
                  J := K;
               end if;

            else
               J := J + 1;
            end if;
         end loop;
      end if;

      --  Fall through for normal case

      Write_Str_With_Col_Check (Name_Buffer (1 .. Name_Len));
   end Write_Name_With_Col_Check;

   ------------------------------------
   -- Write_Name_With_Col_Check_Sloc --
   ------------------------------------

   procedure Write_Name_With_Col_Check_Sloc (N : Name_Id) is
   begin
      Get_Name_String (N);
      Write_Str_With_Col_Check_Sloc (Name_Buffer (1 .. Name_Len));
   end Write_Name_With_Col_Check_Sloc;

   --------------------
   -- Write_Operator --
   --------------------

   procedure Write_Operator (N : Node_Id; S : String) is
      F : Natural := S'First;
      T : Natural := S'Last;

   begin
      --  If no overflow check, just write string out, and we are done

      if not Do_Overflow_Check (N) then
         Write_Str_Sloc (S);

      --  If overflow check, we want to surround the operator with curly
      --  brackets, but not include spaces within the brackets.

      else
         if S (F) = ' ' then
            Write_Char (' ');
            F := F + 1;
         end if;

         if S (T) = ' ' then
            T := T - 1;
         end if;

         Write_Char ('{');
         Write_Str_Sloc (S (F .. T));
         Write_Char ('}');

         if S (S'Last) = ' ' then
            Write_Char (' ');
         end if;
      end if;
   end Write_Operator;

   -----------------------
   -- Write_Param_Specs --
   -----------------------

   procedure Write_Param_Specs (N : Node_Id) is
      Specs  : List_Id;
      Spec   : Node_Id;
      Formal : Node_Id;

   begin
      Specs := Parameter_Specifications (N);

      if Is_Non_Empty_List (Specs) then
         Write_Str_With_Col_Check (" (");
         Spec := First (Specs);

         loop
            Sprint_Node (Spec);
            Formal := Defining_Identifier (Spec);
            Next (Spec);
            exit when Spec = Empty;

            --  Add semicolon, unless we are printing original tree and the
            --  next specification is part of a list (but not the first
            --  element of that list)

            if not Dump_Original_Only or else not Prev_Ids (Spec) then
               Write_Str ("; ");
            end if;
         end loop;

         --  Write out any extra formals

         while Present (Extra_Formal (Formal)) loop
            Formal := Extra_Formal (Formal);
            Write_Str ("; ");
            Write_Name_With_Col_Check (Chars (Formal));
            Write_Str (" : ");
            Write_Name_With_Col_Check (Chars (Etype (Formal)));
         end loop;

         Write_Char (')');
      end if;
   end Write_Param_Specs;

   -----------------------
   -- Write_Rewrite_Str --
   -----------------------

   procedure Write_Rewrite_Str (S : String) is
   begin
      if not Dump_Generated_Only then
         if S'Length = 3 and then S = ">>>" then
            Write_Str (">>>");
         else
            Write_Str_With_Col_Check (S);
         end if;
      end if;
   end Write_Rewrite_Str;

   -----------------------
   -- Write_Source_Line --
   -----------------------

   procedure Write_Source_Line (L : Physical_Line_Number) is
      Loc : Source_Ptr;
      Src : Source_Buffer_Ptr;
      Scn : Source_Ptr;

   begin
      if Dump_Source_Text then
         Src := Source_Text (Current_Source_File);
         Loc := Line_Start (L, Current_Source_File);
         Write_Eol;

         --  See if line is a comment line, if not, and if not line one,
         --  precede with blank line.

         Scn := Loc;
         while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
            Scn := Scn + 1;
         end loop;

         if (Src (Scn) in Line_Terminator
              or else Src (Scn .. Scn + 1) /= "--")
           and then L /= 1
         then
            Write_Eol;
         end if;

         --  Now write the source text of the line

         Write_Str ("-- ");
         Write_Int (Int (L));
         Write_Str (": ");

         while Src (Loc) not in Line_Terminator loop
            Write_Char (Src (Loc));
            Loc := Loc + 1;
         end loop;
      end if;
   end Write_Source_Line;

   ------------------------
   -- Write_Source_Lines --
   ------------------------

   procedure Write_Source_Lines (L : Physical_Line_Number) is
   begin
      while Last_Line_Printed < L loop
         Last_Line_Printed := Last_Line_Printed + 1;
         Write_Source_Line (Last_Line_Printed);
      end loop;
   end Write_Source_Lines;

   --------------------
   -- Write_Str_Sloc --
   --------------------

   procedure Write_Str_Sloc (S : String) is
   begin
      for J in S'Range loop
         Write_Char_Sloc (S (J));
      end loop;
   end Write_Str_Sloc;

   ------------------------------
   -- Write_Str_With_Col_Check --
   ------------------------------

   procedure Write_Str_With_Col_Check (S : String) is
   begin
      if Int (S'Last) + Column > Line_Limit then
         Write_Indent_Str ("  ");

         if S (S'First) = ' ' then
            Write_Str (S (S'First + 1 .. S'Last));
         else
            Write_Str (S);
         end if;

      else
         Write_Str (S);
      end if;
   end Write_Str_With_Col_Check;

   -----------------------------------
   -- Write_Str_With_Col_Check_Sloc --
   -----------------------------------

   procedure Write_Str_With_Col_Check_Sloc (S : String) is
   begin
      if Int (S'Last) + Column > Line_Limit then
         Write_Indent_Str ("  ");

         if S (S'First) = ' ' then
            Write_Str_Sloc (S (S'First + 1 .. S'Last));
         else
            Write_Str_Sloc (S);
         end if;

      else
         Write_Str_Sloc (S);
      end if;
   end Write_Str_With_Col_Check_Sloc;

   -------------------------------
   -- Write_Uint_With_Col_Check --
   -------------------------------

   procedure Write_Uint_With_Col_Check (U : Uint; Format : UI_Format) is
   begin
      Col_Check (UI_Decimal_Digits_Hi (U));
      UI_Write (U, Format);
   end Write_Uint_With_Col_Check;

   ------------------------------------
   -- Write_Uint_With_Col_Check_Sloc --
   ------------------------------------

   procedure Write_Uint_With_Col_Check_Sloc (U : Uint; Format : UI_Format) is
   begin
      Col_Check (UI_Decimal_Digits_Hi (U));
      Set_Debug_Sloc;
      UI_Write (U, Format);
   end Write_Uint_With_Col_Check_Sloc;

   -------------------------------------
   -- Write_Ureal_With_Col_Check_Sloc --
   -------------------------------------

   procedure Write_Ureal_With_Col_Check_Sloc (U : Ureal) is
      D : constant Uint := Denominator (U);
      N : constant Uint := Numerator (U);

   begin
      Col_Check
        (UI_Decimal_Digits_Hi (D) + UI_Decimal_Digits_Hi (N) + 4);
      Set_Debug_Sloc;
      UR_Write (U);
   end Write_Ureal_With_Col_Check_Sloc;

end Sprint;
