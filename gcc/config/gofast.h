/* US Software GOFAST floating point library support.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
  add_optab->handlers[(int) SFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpadd"); \
  add_optab->handlers[(int) DFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpadd"); \
  sub_optab->handlers[(int) SFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpsub"); \
  sub_optab->handlers[(int) DFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpsub"); \
  smul_optab->handlers[(int) SFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpmul"); \
  smul_optab->handlers[(int) DFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpmul"); \
  flodiv_optab->handlers[(int) SFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpdiv"); \
  flodiv_optab->handlers[(int) DFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpdiv"); \
  cmp_optab->handlers[(int) SFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpcmp"); \
  cmp_optab->handlers[(int) DFmode].libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpcmp"); \
\
  extendsfdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fptodp"); \
  truncdfsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dptofp"); \
\
  eqsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpcmp"); \
  nesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpcmp"); \
  gtsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpcmp"); \
  gesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpcmp"); \
  ltsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpcmp"); \
  lesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fpcmp"); \
\
  eqdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpcmp"); \
  nedf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpcmp"); \
  gtdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpcmp"); \
  gedf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpcmp"); \
  ltdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpcmp"); \
  ledf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dpcmp"); \
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
  floatsisf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "sitofp"); \
  floatsidf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "litodp"); \
  fixsfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fptosi"); \
  fixdfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dptoli"); \
  fixunssfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "fptoui"); \
  fixunsdfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "dptoul"); \

/* End of GOFAST_RENAME_LIBCALLS */
