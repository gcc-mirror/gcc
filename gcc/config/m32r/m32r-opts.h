/* Definitions for option handling for Renesas M32R cpu.
   Copyright (C) 1996-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef M32R_OPTS_H
#define M32R_OPTS_H

/* Code Models

   Code models are used to select between two choices of two separate
   possibilities (address space size, call insn to use):

   small: addresses use 24 bits, use bl to make calls
   medium: addresses use 32 bits, use bl to make calls (*1)
   large: addresses use 32 bits, use seth/add3/jl to make calls (*2)

   The fourth is "addresses use 24 bits, use seth/add3/jl to make calls" but
   using this one doesn't make much sense.

   (*1) The linker may eventually be able to relax seth/add3 -> ld24.
   (*2) The linker may eventually be able to relax seth/add3/jl -> bl.

   Internally these are recorded as TARGET_ADDR{24,32} and
   TARGET_CALL{26,32}.

   The __model__ attribute can be used to select the code model to use when
   accessing particular objects.  */

enum m32r_model { M32R_MODEL_SMALL, M32R_MODEL_MEDIUM, M32R_MODEL_LARGE };

#define TARGET_MODEL_SMALL  (m32r_model_selected == M32R_MODEL_SMALL)
#define TARGET_MODEL_MEDIUM (m32r_model_selected == M32R_MODEL_MEDIUM)
#define TARGET_MODEL_LARGE  (m32r_model_selected == M32R_MODEL_LARGE)
#define TARGET_ADDR24       (m32r_model_selected == M32R_MODEL_SMALL)
#define TARGET_ADDR32       (! TARGET_ADDR24)
#define TARGET_CALL26       (! TARGET_CALL32)
#define TARGET_CALL32       (m32r_model_selected == M32R_MODEL_LARGE)

/* The default is the small model.  */
#ifndef M32R_MODEL_DEFAULT
#define M32R_MODEL_DEFAULT M32R_MODEL_SMALL
#endif

/* Small Data Area

   The SDA consists of sections .sdata, .sbss, and .scommon.
   .scommon isn't a real section, symbols in it have their section index
   set to SHN_M32R_SCOMMON, though support for it exists in the linker script.

   Two switches control the SDA:

   -G NNN        - specifies the maximum size of variable to go in the SDA

   -msdata=foo   - specifies how such variables are handled

        -msdata=none  - small data area is disabled

        -msdata=sdata - small data goes in the SDA, special code isn't
                        generated to use it, and special relocs aren't
                        generated

        -msdata=use   - small data goes in the SDA, special code is generated
                        to use the SDA and special relocs are generated

   The SDA is not multilib'd, it isn't necessary.
   MULTILIB_EXTRA_OPTS is set in tmake_file to -msdata=sdata so multilib'd
   libraries have small data in .sdata/SHN_M32R_SCOMMON so programs that use
   -msdata=use will successfully link with them (references in header files
   will cause the compiler to emit code that refers to library objects in
   .data).  ??? There can be a problem if the user passes a -G value greater
   than the default and a library object in a header file is that size.
   The default is 8 so this should be rare - if it occurs the user
   is required to rebuild the libraries or use a smaller value for -G.  */

/* Maximum size of variables that go in .sdata/.sbss.
   The -msdata=foo switch also controls how small variables are handled.  */
#ifndef SDATA_DEFAULT_SIZE
#define SDATA_DEFAULT_SIZE 8
#endif

enum m32r_sdata { M32R_SDATA_NONE, M32R_SDATA_SDATA, M32R_SDATA_USE };

#define TARGET_SDATA_NONE  (m32r_sdata_selected == M32R_SDATA_NONE)
#define TARGET_SDATA_SDATA (m32r_sdata_selected == M32R_SDATA_SDATA)
#define TARGET_SDATA_USE   (m32r_sdata_selected == M32R_SDATA_USE)

/* Default is to disable the SDA
   [for upward compatibility with previous toolchains].  */
#ifndef M32R_SDATA_DEFAULT
#define M32R_SDATA_DEFAULT M32R_SDATA_NONE
#endif

#endif
