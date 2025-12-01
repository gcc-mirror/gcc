/* PR c++/122905 */
/* { dg-lto-do link } */
/* { dg-lto-options { { -O2 -flto -std=c++20 } } } */

#include "pr122905.h"

A
foo ()
{
  return "foo";
}

int
main ()
{
}
