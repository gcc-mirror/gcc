/* US Software GOFAST floating point library support.
   Copyright (C) 1994, 1998, 1999, 2002, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* The US Software GOFAST library requires special optabs support.
   This file is intended to be included by config/ARCH/ARCH.c.  It
   defines one function, gofast_maybe_init_libfuncs, which should be
   called from the TARGET_INIT_LIBFUNCS hook.  When tm.h has defined
   US_SOFTWARE_GOFAST, this function will adjust all the optabs and
   libfuncs appropriately.  Otherwise it will do nothing.  */

static void
gofast_maybe_init_libfuncs (void)
{
#ifdef US_SOFTWARE_GOFAST
  int mode;

  set_optab_libfunc (add_optab, SFmode, "fpadd");
  set_optab_libfunc (add_optab, DFmode, "dpadd");
  set_optab_libfunc (sub_optab, SFmode, "fpsub");
  set_optab_libfunc (sub_optab, DFmode, "dpsub");
  set_optab_libfunc (smul_optab, SFmode, "fpmul");
  set_optab_libfunc (smul_optab, DFmode, "dpmul");
  set_optab_libfunc (sdiv_optab, SFmode, "fpdiv");
  set_optab_libfunc (sdiv_optab, DFmode, "dpdiv");
  set_optab_libfunc (cmp_optab, SFmode, "fpcmp");
  set_optab_libfunc (cmp_optab, DFmode, "dpcmp");

  /* GOFAST does not provide libfuncs for negation, so we use the
     standard names.  */

  /* GCC does not use fpcmp/dpcmp for gt or ge because its own
     FP-emulation library returns +1 for both > and unord.  So we
     leave gt and ge unset, such that, instead of fpcmp(a,b) >[=], we
     generate fpcmp(b,a) <[=] 0, which is unambiguous.  For unord
     libfuncs, we use our own functions, since GOFAST doesn't supply
     them.  */

  set_optab_libfunc (eq_optab, SFmode, "fpcmp");
  set_optab_libfunc (ne_optab, SFmode, "fpcmp");
  set_optab_libfunc (gt_optab, SFmode, 0);
  set_optab_libfunc (ge_optab, SFmode, 0);
  set_optab_libfunc (lt_optab, SFmode, "fpcmp");
  set_optab_libfunc (le_optab, SFmode, "fpcmp");

  set_optab_libfunc (eq_optab, DFmode, "dpcmp");
  set_optab_libfunc (ne_optab, DFmode, "dpcmp");
  set_optab_libfunc (gt_optab, DFmode, 0);
  set_optab_libfunc (ge_optab, DFmode, 0);
  set_optab_libfunc (lt_optab, DFmode, "dpcmp");
  set_optab_libfunc (le_optab, DFmode, "dpcmp");

  extendsfdf2_libfunc = init_one_libfunc ("fptodp");
  truncdfsf2_libfunc = init_one_libfunc ("dptofp");

  floatsisf_libfunc = init_one_libfunc ("sitofp");
  floatsidf_libfunc = init_one_libfunc ("litodp");
  fixsfsi_libfunc = init_one_libfunc ("fptosi");
  fixdfsi_libfunc = init_one_libfunc ("dptoli");
  fixunssfsi_libfunc = init_one_libfunc ("fptoui");
  fixunsdfsi_libfunc = init_one_libfunc ("dptoul");
#endif
}
