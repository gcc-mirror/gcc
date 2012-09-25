/* { dg-lto-do link } */
/* { dg-lto-options { { -O2 -flto -w } } } */
/* { dg-extra-ld-options { -r -nostdlib } } */

#include <stdlib.h>
void* f ()
{
  void* p = malloc (1);
  return p;
}
