/* Bug 117164: ICE with -std=gnu23 inlining function containing call
   to non-inlined nested function returning variable-size struct.  */
/* { dg-do run } */
/* { dg-options "-O3 --param ipa-cp-eval-threshold=0 -fno-guess-branch-probability -std=gnu23" } */
/* { dg-require-effective-target alloca } */

#include "nested-func-16.c"
