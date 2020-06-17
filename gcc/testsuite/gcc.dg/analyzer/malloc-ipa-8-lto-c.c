/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-additional-options "-flto" } */
/* { dg-additional-sources "malloc-ipa-8-lto-a.c malloc-ipa-8-lto-b.c" } */

#include <stdlib.h>
#include "malloc-ipa-8-lto.h"

void test (int i)
{
  boxed_int *obj = make_boxed_int (i);

  free_boxed_int (obj);
  free (obj); /* { dg-warning "double-free" } */
}

int main() { return 0; }
