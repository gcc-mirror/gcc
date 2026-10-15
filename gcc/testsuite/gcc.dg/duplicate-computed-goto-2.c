/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-options "-O2 -fno-tree-slp-vectorize --param=max-goto-duplication-insns=64 -fdump-rtl-compgotos" } */

/* The computed goto of duplicate-computed-goto-1.c is duplicated with a
   sufficient budget.  */

#include "pr127246-1.c"

/* { dg-final { scan-rtl-dump "Duplicating" "compgotos" } } */
