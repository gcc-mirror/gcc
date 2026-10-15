/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-options "-O2 -fduplicate-switch-dispatch -mcmodel=tiny -fno-pie -fselective-scheduling2 -fsel-sched-pipelining -fno-gcse -fno-move-loop-invariants -fdump-rtl-compgotos-details" } */

/* Keep the table address in the dispatch blocks.  Selective scheduling must
   not redirect shared-table edges to create loop preheaders.  */

#include "switch-dispatch-1.c"

/* { dg-final { scan-rtl-dump "Duplicating dispatch" "compgotos" } } */
/* { dg-final { scan-assembler-times "\\.Lrtx\[0-9\]+:" 1 } } */
