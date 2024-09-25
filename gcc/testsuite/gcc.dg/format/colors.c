/* Verify that colorization affects both text within diagnostic messages
   and underlined ranges of quoted source, and that the types we use
   match up between them.  */

/* { dg-do compile } */
/* { dg-options "-Wformat -fdiagnostics-show-caret -fdiagnostics-color=always" } */

#include "format.h"

void test_mismatching_types (const char *msg)
{
  printf("hello %i", msg);

/* { dg-begin-multiline-output "" }
warning: [m[Kformat '[01m[K[01;32m[K%i[m[K[m[K' expects argument of type '[01m[K[01;32m[Kint[m[K[m[K', but argument 2 has type '[01m[K[01;34m[Kconst char *[m[K[m[K' [[01;35m[K-Wformat=[m[K]
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   printf("hello [01;32m[K%[m[K[01;32m[Ki[m[K", [01;34m[Km[m[K[01;34m[Ks[m[K[01;34m[Kg[m[K);
                 [01;32m[K~[m[K[01;32m[K^[m[K   [01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K
                  [01;32m[K|[m[K   [01;34m[K|[m[K
                  [01;32m[Kint[m[K [01;34m[Kconst char *[m[K
                 [32m[K%s[m[K
   { dg-end-multiline-output "" } */
}

/* { dg-prune-output "In function" } */
/* { dg-prune-output "colors.c" } */
