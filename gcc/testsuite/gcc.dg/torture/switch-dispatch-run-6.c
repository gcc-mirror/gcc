/* PR middle-end/126236 */
/* { dg-do run { target fpic } } */
/* { dg-require-effective-target size20plus } */
/* { dg-additional-options "-fpic -fduplicate-switch-dispatch --param=max-goto-duplication-insns=128 --param=case-values-threshold=1" } */

/* PIC dispatches, with copies far from their tables.  */

#define LARGE
#include "switch-dispatch-run-5.c"
