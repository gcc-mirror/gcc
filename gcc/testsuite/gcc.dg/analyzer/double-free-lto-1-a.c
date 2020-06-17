/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-additional-options "-flto" } */
/* { dg-additional-sources double-free-lto-1-b.c } */

#include <stdlib.h>
#include "double-free-lto-1.h"

void test (void *ptr)
{
  calls_free (ptr); /* { dg-message "calling 'calls_free' from 'test'" } */
  free (ptr); /* { dg-warning "double-'free' of 'ptr_.+'" } */
  // TODO: report "ptr", rather than an SSA name
}

int main() { return 0; }
