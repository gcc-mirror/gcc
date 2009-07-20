------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S P R I N T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This package (source print) contains routines for printing the source
--  program corresponding to a specified syntax tree. These routines are
--  intended for debugging use in the compiler (not as a user level pretty
--  print tool). Only information present in the tree is output (e.g. no
--  comments are present in the output), and as far as possible we avoid
--  making any assumptions about the correctness of the tree, so a bad
--  tree may either blow up on a debugging check, or list incorrect source.

with Types; use Types;
package Sprint is

   -----------------------
   -- Syntax Extensions --
   -----------------------

   --  When the generated tree is printed, it contains constructs that are not
   --  pure Ada. For convenience, syntactic extensions to Ada have been defined
   --  purely for the purposes of this printout (they are not recognized by the
   --  parser)

   --  Could use more documentation for all of these ???

   --    Allocator                           new xxx [storage_pool = xxx]
   --    Cleanup action                      at end procedure name;
   --    Conditional expression              (if expr then expr else expr)
   --    Conversion wi Float_Truncate        target^(source)
   --    Convert wi Conversion_OK            target?(source)
   --    Convert wi Rounded_Result           target@(source)
   --    Divide wi Treat_Fixed_As_Integer    x #/ y
   --    Divide wi Rounded_Result            x @/ y
   --    Expression with range check         {expression}
   --    Operator with range check           {operator} (e.g. {+})
   --    Free statement                      free expr [storage_pool = xxx]
   --    Freeze entity with freeze actions   freeze entityname [ actions ]
   --    Implicit call to run time routine   $routine-name
   --    Implicit exportation                $pragma import (...)
   --    Implicit importation                $pragma export (...)
   --    Interpretation                      interpretation type [, entity]
   --    Intrinsic calls                     function-name!(arg, arg, arg)
   --    Itype declaration                   [(sub)type declaration without ;]
   --    Itype reference                     reference itype
   --    Label declaration                   labelname : label
   --    Mod wi Treat_Fixed_As_Integer       x #mod y
   --    Multiple concatenation              expr && expr && expr ... && expr
   --    Multiply wi Treat_Fixed_As_Integer  x #* y
   --    Multiply wi Rounded_Result          x @* y
   --    Others choice for cleanup           when all others
   --    Pop exception label                 %pop_xxx_exception_label
   --    Push exception label                %push_xxx_exception_label (label)
   --    Raise xxx error                     [xxx_error [when cond]]
   --    Raise xxx error with msg            [xxx_error [when cond], "msg"]
   --    Rational literal                    See UR_Write for details
   --    Rem wi Treat_Fixed_As_Integer       x #rem y
   --    Reference                           expression'reference
   --    Shift nodes                         shift_name!(expr, count)
   --    Static declaration                  name : static xxx
   --    Subprogram_Info                     subprog'Subprogram_Info
   --    Unchecked conversion                target_type!(source_expression)
   --    Unchecked expression                `(expression)
   --    Validate_Unchecked_Conversion       validate unchecked_conversion
   --                                                  (src-type, target-typ);

   --  Note: the storage_pool parameters for allocators and the free node are
   --  omitted if the Storage_Pool field is Empty, indicating use of the
   --  standard default pool.

   -----------------
   -- Subprograms --
   -----------------

   procedure Source_Dump;
   --  This routine is called from the GNAT main program to dump source as
   --  requested by debug options. The relevant debug options are:
   --    -ds  print source from tree, both original and generated code
   --    -dg  print source from tree, including only the generated code
   --    -do  print source from tree, including only the original code
   --    -df  modify the above to include all units, not just the main unit
   --    -sz  print source from tree for package Standard

   procedure Sprint_Comma_List (List : List_Id);
   --  Prints the nodes in a list, with separating commas. If the list is empty
   --  then no output is generated.

   procedure Sprint_Paren_Comma_List (List : List_Id);
   --  Prints the nodes in a list, surrounded by parentheses, and separated by
   --  commas. If the list is empty, then no output is generated. A blank is
   --  output before the initial left parenthesis.

   procedure Sprint_Opt_Paren_Comma_List (List : List_Id);
   --  Same as normal Sprint_Paren_Comma_List procedure, except that an extra
   --  blank is output if List is non-empty, and nothing at all is printed it
   --  the argument is No_List.

   procedure Sprint_Node_List (List : List_Id);
   --  Prints the nodes in a list with no separating characters. This is used
   --  in the case of lists of items which are printed on separate lines using
   --  the current indentation amount. Note that Sprint_Node_List itself
   --  does not generate any New_Line calls.

   procedure Sprint_Opt_Node_List (List : List_Id);
   --  Like Sprint_Node_List, but prints nothing if List = No_List

   procedure Sprint_Indented_List (List : List_Id);
   --  Like Sprint_Line_List, except that the indentation level is increased
   --  before outputting the list of items, and then decremented (back to its
   --  original level) before returning to the caller.

   procedure Sprint_Node (Node : Node_Id);
   --  Prints a single node. No new lines are output, except as required for
   --  splitting lines that are too long to fit on a single physical line.
   --  No output is generated at all if Node is Empty. No trailing or leading
   --  blank characters are generated.

   procedure Sprint_Opt_Node (Node : Node_Id);
   --  Same as normal Sprint_Node procedure, except that one leading blank is
   --  output before the node if it is non-empty.

   procedure pg (Arg : Union_Id);
   pragma Export (Ada, pg);
   --  Print generated source for argument N (like -gnatdg output). Intended
   --  only for use from gdb for debugging purposes. Currently, Arg may be a
   --  List_Id or a Node_Id (anything else outputs a blank line).

   procedure po (Arg : Union_Id);
   pragma Export (Ada, po);
   --  Like pg, but prints original source for the argument (like -gnatdo
   --  output). Intended only for use from gdb for debugging purposes.

   procedure ps (Arg : Union_Id);
   pragma Export (Ada, ps);
   --  Like pg, but prints generated and original source for the argument (like
   --  -gnatds output). Intended only for use from gdb for debugging purposes.

end Sprint;
