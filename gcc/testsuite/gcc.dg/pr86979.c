/* { dg-options "-Og -fPIC -fschedule-insns2 -fselective-scheduling2 -fno-tree-fre --param=max-sched-extend-regions-iters=2" } */
/* { dg-require-effective-target scheduling } */
/* { dg-require-effective-target fpic } */

#include "../gcc.c-torture/compile/pr69102.c"
