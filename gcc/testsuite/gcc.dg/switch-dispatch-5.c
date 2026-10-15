/* PR middle-end/126236 */
/* { dg-do run } */
/* { dg-options "-O2 --param=max-goto-duplication-insns=128 --param=case-values-threshold=1 -fdump-rtl-compgotos-details" } */

/* -O2 enables -fduplicate-switch-dispatch, so both tables of
   switch-dispatch-run-5.c are shared.  */

#include "torture/switch-dispatch-run-5.c"

/* { dg-final { scan-rtl-dump-times {addr_(?:diff_)?vec/u} 2 "compgotos" { target shared_jump_tables } } } */
