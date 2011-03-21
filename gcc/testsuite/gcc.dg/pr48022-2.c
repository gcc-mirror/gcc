/* PR tree-optimization/48022 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-overflow" } */

#include <string.h>

int
foo (const char *x)
{
  return strcmp (x, "/");
}
