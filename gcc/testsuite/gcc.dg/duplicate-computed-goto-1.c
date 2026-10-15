/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-options "-O2 -fno-tree-slp-vectorize --param=max-goto-duplication-insns=0 -fdump-rtl-compgotos" } */

/* A zero budget disables computed goto duplication, also on targets
   without an insn length attribute.  */

#include "pr127246-1.c"

/* { dg-final { scan-rtl-dump-not "Duplicating" "compgotos" } } */
