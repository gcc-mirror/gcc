/* Test nested-func-12.c with -std=gnu17.  */
/* { dg-do run } */
/* { dg-options "-Ofast --param ipa-cp-eval-threshold=0 -fno-guess-branch-probability -fno-inline-small-functions -std=gnu17" } */

#include "nested-func-12.c"
