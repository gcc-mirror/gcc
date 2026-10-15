/* PR middle-end/126236 */
/* { dg-do run } */
/* { dg-require-effective-target size20plus } */
/* { dg-additional-options "-fno-pic -fduplicate-switch-dispatch --param=max-goto-duplication-insns=128 --param=case-values-threshold=1" } */
/* { dg-additional-options "-no-pie" { target pie_enabled } } */
/* { dg-skip-if "" { arm*-*-uclinuxfdpiceabi } "*" "" } */

/* Non-PIC dispatches, with copies far from their tables.  */

#define LARGE
#include "switch-dispatch-run-5.c"
