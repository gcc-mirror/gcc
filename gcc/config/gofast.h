/* US Software GOFAST floating point library support.
   Copyright (C) 1994, 1998, 1999 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This is used by fp-bit.c.  */
#define US_SOFTWARE_GOFAST

/* The US Software GOFAST library requires special optabs support.
   There is no negation libcall, and several others have names different
   from gcc.  This file consolidates the support in one place.

   The basic plan is to leave gcc proper alone and via some hook fix things
   after the optabs have been set up.  Our main entry point is
   INIT_GOFAST_OPTABS.  */

#define INIT_GOFAST_OPTABS \
  do { \
    GOFAST_CLEAR_NEG_FLOAT_OPTAB; \
    GOFAST_RENAME_LIBCALLS; \
  } while (0)

#define GOFAST_CLEAR_NEG_FLOAT_OPTAB \
  do { \
    int mode; \
    for (mode = SFmode; (int) mode <= (int) TFmode; \
	 mode = (enum machine_mode) ((int) mode + 1)) \
      neg_optab->handlers[(int) mode].libfunc = NULL_RTX; \
  } while (0)

#define GOFAST_RENAME_LIBCALLS \
  add_optab->handlers[(int) SFmode].libfunc = init_one_libfunc ("fpadd"); \
  add_optab->handlers[(int) DFmode].libfunc = init_one_libfunc ("dpadd"); \
  sub_optab->handlers[(int) SFmode].libfunc = init_one_libfunc ("fpsub"); \
  sub_optab->handlers[(int) DFmode].libfunc = init_one_libfunc ("dpsub"); \
  smul_optab->handlers[(int) SFmode].libfunc = init_one_libfunc ("fpmul"); \
  smul_optab->handlers[(int) DFmode].libfunc = init_one_libfunc ("dpmul"); \
  sdiv_optab->handlers[(int) SFmode].libfunc = init_one_libfunc ("fpdiv"); \
  sdiv_optab->handlers[(int) DFmode].libfunc = init_one_libfunc ("dpdiv"); \
  cmp_optab->handlers[(int) SFmode].libfunc = init_one_libfunc ("fpcmp"); \
  cmp_optab->handlers[(int) DFmode].libfunc = init_one_libfunc ("dpcmp"); \
\
  extendsfdf2_libfunc = init_one_libfunc ("fptodp"); \
  truncdfsf2_libfunc = init_one_libfunc ("dptofp"); \
\
  eqsf2_libfunc = init_one_libfunc ("fpcmp"); \
  nesf2_libfunc = init_one_libfunc ("fpcmp"); \
  gtsf2_libfunc = init_one_libfunc ("fpcmp"); \
  gesf2_libfunc = init_one_libfunc ("fpcmp"); \
  ltsf2_libfunc = init_one_libfunc ("fpcmp"); \
  lesf2_libfunc = init_one_libfunc ("fpcmp"); \
\
  eqdf2_libfunc = init_one_libfunc ("dpcmp"); \
  nedf2_libfunc = init_one_libfunc ("dpcmp"); \
  gtdf2_libfunc = init_one_libfunc ("dpcmp"); \
  gedf2_libfunc = init_one_libfunc ("dpcmp"); \
  ltdf2_libfunc = init_one_libfunc ("dpcmp"); \
  ledf2_libfunc = init_one_libfunc ("dpcmp"); \
\
  eqxf2_libfunc = NULL_RTX; \
  nexf2_libfunc = NULL_RTX; \
  gtxf2_libfunc = NULL_RTX; \
  gexf2_libfunc = NULL_RTX; \
  ltxf2_libfunc = NULL_RTX; \
  lexf2_libfunc = NULL_RTX; \
\
  eqtf2_libfunc = NULL_RTX; \
  netf2_libfunc = NULL_RTX; \
  gttf2_libfunc = NULL_RTX; \
  getf2_libfunc = NULL_RTX; \
  lttf2_libfunc = NULL_RTX; \
  letf2_libfunc = NULL_RTX; \
\
  floatsisf_libfunc = init_one_libfunc ("sitofp"); \
  floatsidf_libfunc = init_one_libfunc ("litodp"); \
  fixsfsi_libfunc = init_one_libfunc ("fptosi"); \
  fixdfsi_libfunc = init_one_libfunc ("dptoli"); \
  fixunssfsi_libfunc = init_one_libfunc ("fptoui"); \
  fixunsdfsi_libfunc = init_one_libfunc ("dptoul"); \

/* End of GOFAST_RENAME_LIBCALLS */
