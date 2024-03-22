/* Definitions for specs for GNU Modula-2.
   Copyright (C) 2001-2024 Free Software Foundation, Inc.
   Contributed by Gaius Mulley.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This is the contribution to the `default_compilers' array in gcc.c for
   GNU Modula-2.  */

/* A spec for the 'integrated' preprocessor implementation for Modula-2.  */
#define M2CPP \
  "%{E|M|MM|fcpp: %{E} %{MF} -fcpp-begin " \
  "      %{!E:-E} %(cpp_unique_options) -traditional-cpp -ansi " \
  "      -fcpp-end %{B*} %{save-temps*} ; \
     : %{v} %I %{B*} %{save-temps*} } "

#define MDMMD \
  " %{MD:-MD %{!o:%b.d}%{o*:%.d%*} %{!MT:-MT %b%O} %{MT} %{MQ} %{MF}} " \
  " %{MMD:-MMD %{!o:%b.d}%{o*:%.d%*} %{!MT:-MT %b%O} %{MT} %{MQ} %{MF}} "

/* We have three modes:
   1. When the preprocessing step is explict and there is no following
      compilation.  Here we do a similar process to cc1 -E where most of
      the compilation is short-circuited.
   2. When we are mimicking an integrated preprocessor.  Here we use the
      modula-2 'fcpp' to construct a command line for the preprocessor and
      snarf save-temps and dumpdir inputs to try and be consistent.
   3. We can consume a pre-processed modula-2 source.  */

  {".mod", "@modula-2", 0, 0, 0},
  {"@modula-2",
   /* For preprocessing we use cc1 but wrap it in cc1gm2.  */
   "%{E|M|MM:\
      cc1gm2 " M2CPP " %{!fcpp:-fcpp;:%{fcpp}} %{fm2-pathname*} %i } \
    %{!E:%{!M:%{!MM:\
      cc1gm2 " M2CPP MDMMD " %(cc1_options) %{fm2-pathname*} %i %{c} \
      %{!fsyntax-only:%(invoke_as)} \
    }}}", 0, 0, 0},
  {".m2i", "@modula-2-cpp-output", 0, 0, 0},
  {"@modula-2-cpp-output",
   "%{!M:%{!MM:%{!E: \
      cc1gm2 %<fcpp %(cc1_options) %{v} %I -fmod=.mod.m2i -fdef=.def.m2i \
        %{fm2-pathname*} \
	-fpreprocessed %i %{c} \
    %{!fsyntax-only:%(invoke_as)}}}}", 0, 0, 0},
