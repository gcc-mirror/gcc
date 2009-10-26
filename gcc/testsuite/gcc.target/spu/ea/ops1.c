/* Copyright (C) 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This is the same as ops2.c except for the compile option.
   If you modify this code, please modify ops2.c as well.  */

/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99 -pedantic-errors -maddress-space-conversion" } */

#define __lm

__ea int ea_var = 1;
__lm int lm_var = 2;

typedef __ea int *ea_ptr_t;
typedef __lm int *lm_ptr_t;

typedef __ea void *ea_vptr_t;
typedef __lm void *lm_vptr_t;

ea_ptr_t ea, ea2;
lm_ptr_t lm, lm2;

ea_vptr_t eav;
lm_vptr_t lmv;

extern void call_ea (ea_ptr_t);
extern void call_lm (lm_ptr_t);

/* Assignment, initialization, argument passing, and return.  */
void to_ea (void) { ea = lm; }
void to_lm (void) { lm = ea; }			/* { dg-error "assignment from pointer to non-enclosed address space" } */
void init_ea (void) { ea_ptr_t l_ea = lm; }
void init_lm (void) { lm_ptr_t l_lm = ea; }	/* { dg-error "initialization from pointer to non-enclosed address space" } */
ea_ptr_t ret_ea (void) { return lm; }
lm_ptr_t ret_lm (void) { return ea; }		/* { dg-error "return from pointer to non-enclosed address space" } */
void call_ea2 (void) { call_ea (lm); }
void call_lm2 (void) { call_lm (ea); }		/* { dg-error "passing argument 1 of 'call_lm' from pointer to non-enclosed address space" } */

/* Explicit casts.  */
void to_ea_with_cast (void) { ea = (ea_ptr_t)lm; }
void to_lm_with_cast (void) { lm = (lm_ptr_t)ea; }
void init_ea_with_cast (void) { ea_ptr_t l_ea = (ea_ptr_t)lm; }
void init_lm_with_cast (void) { lm_ptr_t l_lm = (lm_ptr_t)ea; }
ea_ptr_t ret_ea_with_cast (void) { return (ea_ptr_t)lm; }
lm_ptr_t ret_lm_with_cast (void) { return (lm_ptr_t)ea; }
void call_ea2_with_cast (void) { call_ea ((ea_ptr_t)lm); }
void call_lm2_with_cast (void) { call_lm ((lm_ptr_t)ea); }

/* Arithmetic operators.  */
int sub_eaea (void) { return ea - ea2; }
int sub_ealm (void) { return ea - lm2; }
int sub_lmea (void) { return lm - ea2; }
int sub_lmlm (void) { return lm - lm2; }
ea_ptr_t if_eaea1 (int test) { return test? ea : ea2; }
lm_ptr_t if_eaea2 (int test) { return test? ea : ea2; }	/* { dg-error "return from pointer to non-enclosed address space" } */
ea_ptr_t if_ealm1 (int test) { return test? ea : lm2; }
lm_ptr_t if_ealm2 (int test) { return test? ea : lm2; }	/* { dg-error "return from pointer to non-enclosed address space" } */
ea_ptr_t if_lmea1 (int test) { return test? lm : ea2; }
lm_ptr_t if_lmea2 (int test) { return test? lm : ea2; }	/* { dg-error "return from pointer to non-enclosed address space" } */
ea_ptr_t if_lmlm1 (int test) { return test? lm : lm2; }
lm_ptr_t if_lmlm2 (int test) { return test? lm : lm2; }

/* Relational operators.  */
int eq_eaea (void) { return ea == ea2; }
int eq_ealm (void) { return ea == lm2; }
int eq_lmea (void) { return lm == ea2; }
int eq_lmlm (void) { return lm == lm2; }
int lt_eaea (void) { return ea < ea2; }
int lt_ealm (void) { return ea < lm2; }
int lt_lmea (void) { return lm < ea2; }
int lt_lmlm (void) { return lm < lm2; }

/* Null pointer.  */
void null_ea1 (void) { ea = 0; }
void null_ea2 (void) { ea = (void *)0; }
void null_ea3 (void) { ea = (__ea void *)0; }
void null_lm1 (void) { lm = 0; }
void null_lm2 (void) { lm = (void *)0; }
void null_lm3 (void) { lm = (__ea void *)0; }	/* { dg-error "assignment from pointer to non-enclosed address space" } */

