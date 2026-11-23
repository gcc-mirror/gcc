/* PR rtl-optimization/128860 */
/* The region size estimate must not depend on debug insns.  A block that
   contains only debug insns used to be counted as -1 insns with -g.  */
/* { dg-do compile } */
/* { dg-require-effective-target scheduling } */
/* { dg-options "-O2 -fschedule-insns -fcompare-debug -w --param max-sched-extend-regions-iters=2 --param max-sched-region-insns=7" } */

#include "torture/pr71802.c"
