------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A L L O C                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains definitions for initial sizes and growth increments
--  for the various dynamic arrays used for principle compiler data strcutures.
--  The indicated initial size is allocated for the start of each file, and
--  the increment factor is a percentage used to increase the table size when
--  it needs expanding (e.g. a value of 100 = 100% increase = double)

--  Note: the initial values here are multiplied by Table_Factor, as set
--  by the -gnatTnn switch. This variable is defined in Opt, as is the
--  default value for the table factor.

package Alloc is

   --  The comment shows the unit in which the table is defined

   All_Interp_Initial               : constant := 1_000;   -- Sem_Type
   All_Interp_Increment             : constant := 100;

   Branches_Initial                 : constant := 1_000;   -- Sem_Warn
   Branches_Increment               : constant := 100;

   Conditionals_Initial             : constant := 1_000;   -- Sem_Warn
   Conditionals_Increment           : constant := 100;

   Conditional_Stack_Initial        : constant := 50;      -- Sem_Warn
   Conditional_Stack_Increment      : constant := 100;

   Elists_Initial                   : constant := 200;     -- Elists
   Elists_Increment                 : constant := 100;

   Elmts_Initial                    : constant := 1_200;   -- Elists
   Elmts_Increment                  : constant := 100;

   Entity_Suppress_Initial          : constant := 100;     -- Sem
   Entity_Suppress_Increment        : constant := 200;

   Inlined_Bodies_Initial           : constant := 50;      -- Inline
   Inlined_Bodies_Increment         : constant := 200;

   Inlined_Initial                  : constant := 100;     -- Inline
   Inlined_Increment                : constant := 100;

   Interp_Map_Initial               : constant := 200;     -- Sem_Type
   Interp_Map_Increment             : constant := 100;

   Lines_Initial                    : constant := 500;     -- Sinput
   Lines_Increment                  : constant := 150;

   Linker_Option_Lines_Initial      : constant := 5;       -- Lib
   Linker_Option_Lines_Increment    : constant := 200;

   Lists_Initial                    : constant := 4_000;   -- Nlists
   Lists_Increment                  : constant := 200;

   Load_Stack_Initial               : constant := 10;      -- Lib
   Load_Stack_Increment             : constant := 100;

   Name_Chars_Initial               : constant := 50_000;  -- Namet
   Name_Chars_Increment             : constant := 100;

   Name_Qualify_Units_Initial       : constant := 200;     -- Exp_Dbug
   Name_Qualify_Units_Increment     : constant := 300;

   Names_Initial                    : constant := 6_000;   -- Namet
   Names_Increment                  : constant := 100;

   Nodes_Initial                    : constant := 50_000;  -- Atree
   Nodes_Increment                  : constant := 100;

   Orig_Nodes_Initial               : constant := 50_000;  -- Atree
   Orig_Nodes_Increment             : constant := 100;

   Pending_Instantiations_Initial   : constant := 10;      -- Inline
   Pending_Instantiations_Increment : constant := 100;

   Rep_Table_Initial                : constant := 1000;    -- Repinfo
   Rep_Table_Increment              : constant := 200;

   Scope_Stack_Initial              : constant := 10;      -- Sem
   Scope_Stack_Increment            : constant := 200;

   SFN_Table_Initial                : constant := 10;      -- Fname
   SFN_Table_Increment              : constant := 200;

   Source_File_Initial              : constant := 10;      -- Sinput
   Source_File_Increment            : constant := 200;

   String_Chars_Initial             : constant := 2_500;   -- Stringt
   String_Chars_Increment           : constant := 150;

   Strings_Initial                  : constant := 5_00;    -- Stringt
   Strings_Increment                : constant := 150;

   Successors_Initial               : constant := 2_00;    -- Inline
   Successors_Increment             : constant := 100;

   Udigits_Initial                  : constant := 10_000;  -- Uintp
   Udigits_Increment                : constant := 100;

   Uints_Initial                    : constant := 5_000;   -- Uintp
   Uints_Increment                  : constant := 100;

   Units_Initial                    : constant := 30;      -- Lib
   Units_Increment                  : constant := 100;

   Ureals_Initial                   : constant := 200;     -- Urealp
   Ureals_Increment                 : constant := 100;

   Unreferenced_Entities_Initial    : constant := 1_000;   -- Sem_Warn
   Unreferenced_Entities_Increment  : constant := 100;

   With_List_Initial                : constant := 10;      -- Features
   With_List_Increment              : constant := 300;

   Xrefs_Initial                    : constant := 5_000;   -- Cross-refs
   Xrefs_Increment                  : constant := 300;

end Alloc;
