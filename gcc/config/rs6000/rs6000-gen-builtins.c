/* Generate built-in function initialization and recognition for Power.
   Copyright (C) 2020-21 Free Software Foundation, Inc.
   Contributed by Bill Schmidt, IBM <wschmidt@linux.ibm.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This program generates built-in function initialization and
   recognition code for Power targets, based on text files that
   describe the built-in functions and vector overloads:

     rs6000-builtin-new.def     Table of built-in functions
     rs6000-overload.def        Table of overload functions

   Both files group similar functions together in "stanzas," as
   described below.

   Each stanza in the built-in function file starts with a line
   identifying the circumstances in which the group of functions is
   permitted, with the gating predicate in square brackets.  For
   example, this could be

     [altivec]

   or it could be

     [power9]

   The bracketed gating predicate is the only information allowed on
   the stanza header line, other than whitespace.

   Following the stanza header are two lines for each function: the
   prototype line and the attributes line.  The prototype line has
   this format, where the square brackets indicate optional
   information and angle brackets indicate required information:

     [kind] <return-type> <bif-name> (<argument-list>);

   Here [kind] can be one of "const", "pure", or "fpmath";
   <return-type> is a legal type for a built-in function result;
   <bif-name> is the name by which the function can be called;
   and <argument-list> is a comma-separated list of legal types
   for built-in function arguments.  The argument list may be
   empty, but the parentheses and semicolon are required.

   The attributes line looks like this:

     <bif-id> <bif-pattern> {<attribute-list>}

   Here <bif-id> is a unique internal identifier for the built-in
   function that will be used as part of an enumeration of all
   built-in functions; <bif-pattern> is the define_expand or
   define_insn that will be invoked when the call is expanded;
   and <attribute-list> is a comma-separated list of special
   conditions that apply to the built-in function.  The attribute
   list may be empty, but the braces are required.

   Attributes are strings, such as these:

     init     Process as a vec_init function
     set      Process as a vec_set function
     extract  Process as a vec_extract function
     nosoft   Not valid with -msoft-float
     ldvec    Needs special handling for vec_ld semantics
     stvec    Needs special handling for vec_st semantics
     reve     Needs special handling for element reversal
     pred     Needs special handling for comparison predicates
     htm      Needs special handling for transactional memory
     htmspr   HTM function using an SPR
     htmcr    HTM function using a CR
     mma      Needs special handling for MMA instructions
     quad     MMA instruction using a register quad as an input operand
     pair     MMA instruction using a register pair as an input operand
     no32bit  Not valid for TARGET_32BIT
     32bit    Requires different handling for TARGET_32BIT
     cpu      This is a "cpu_is" or "cpu_supports" builtin
     ldstmask Altivec mask for load or store
     lxvrse   Needs special handling for load-rightmost, sign-extended
     lxvrze   Needs special handling for load-rightmost, zero-extended
     endian   Needs special handling for endianness

   An example stanza might look like this:

[altivec]
  const vsc __builtin_altivec_abs_v16qi (vsc);
    ABS_V16QI absv16qi2 {}
  const vss __builtin_altivec_abs_v8hi (vss);
    ABS_V8HI absv8hi2 {}

   Here "vsc" and "vss" are shorthand for "vector signed char" and
   "vector signed short" to shorten line lengths and improve readability.
   Note the use of indentation, which is recommended but not required.

   The overload file has more complex stanza headers.  Here the stanza
   represents all functions with the same overloaded function name:

     [<overload-id>, <abi-name>, <builtin-name>[[, <ifdef>]] ]

   Here the single square brackets are part of the syntax, <overload-id>
   is a unique internal identifier for the overload that will be used as
   part of an enumeration of all overloaded functions; <abi-name> is the
   name that will appear as a #define in rs6000-vecdefines.h;
   <builtin-name> is the name that is overloaded in the back end; and
   <ifdef> is an optional token used to guard the #define with an #ifdef
   in rs6000-vecdefines.h.

   Each function entry again has two lines.  The first line is again a
   prototype line (this time without [kind]):

     <return-type> <internal-name> (<argument-list>);

   The second line contains the <bif-id> that this particular instance of
   the overloaded function maps to.  It must match a token that appears in
   rs6000-builtin-new.def.  Optionally, a second token may appear.  If only
   one token is on the line, it is also used to build the unique identifier
   for the overloaded function.  If a second token is present, the second
   token is used instead for this purpose.  This is necessary in cases
   where a built-in function accepts more than one type signature.  It is
   common to have a built-in function that, for example, specifies a
   "vector signed char" argument, but accepts "vector unsigned char" and
   "vector bool char" as well because only the mode matters.  Note that
   the overload resolution mechanism has always handled these cases by
   performing fold_convert on vector arguments to hide type mismatches,
   and it will continue to do so.

   As a concrete example, __builtin_altivec_mtvscr uses an opaque argument
   type for the source operand.  Its built-in function id is MTVSCR.  The
   overloaded function __builtin_vec_mtvscr takes a variety of specific
   types, but not all vector types.  Each of these maps to the same
   __builtin_altivec_mtvscr built-in function, but the overload ID must
   be unique, so we must specify the second token as shown here.

    [VEC_MTVSCR, vec_mtvscr, __builtin_vec_mtvscr]
      void __builtin_vec_mtvscr (vbc);
	MTVSCR  MTVSCR_VBC
      void __builtin_vec_mtvscr (vsc);
	MTVSCR  MTVSCR_VSC
      ...

  Blank lines may be used as desired in these files between the lines as
  defined above; that is, you can introduce as many extra newlines as you
  like after a required newline, but nowhere else.  Lines beginning with
  a semicolon are also treated as blank lines.  */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
