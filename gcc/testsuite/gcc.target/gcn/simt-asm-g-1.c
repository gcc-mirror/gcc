/* Ensure that a "g" asm operand can hold a per-lane value in SIMT mode.

   "g" is "r" plus memory plus immediates, and its register half means
   GENERAL_REGS, which on this target is the scalar registers.  A 64-lane
   value cannot satisfy that, so unless the constraint is rewritten to
   "vmi" (see gcn_md_asm_adjust) this fails to compile with "inconsistent
   operand constraints in an 'asm'", which makes inline asm that uses "g"
   -- in the libraries, or in user code -- unusable in SIMT mode.

   See simt-asm-r-1.c for the same thing with "r".  */

/* { dg-do compile } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)

/* Global variables aren't thread-local.  */
int a[64];

void
f (void)
{
  int A, x;

  A = __builtin_gcn_simt_enter (1, 2);
  x = a[TID];
  __asm__ volatile ("; use %0" :: "g"(x));
  a[TID] = x + 1;
  __builtin_gcn_simt_exit (A);
}

/* The value is per-lane, so it has to end up in a VGPR.  */
/* { dg-final { scan-assembler "; use v\[0-9\]+" } } */
