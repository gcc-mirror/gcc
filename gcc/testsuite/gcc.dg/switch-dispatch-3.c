/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-options "-O2 -fduplicate-switch-dispatch --param=max-goto-duplication-insns=0 -fdump-rtl-compgotos-details" } */

#include "torture/switch-dispatch-run-1.c"

/* { dg-final { scan-rtl-dump-not "Duplicating dispatch" "compgotos" } } */
