/* { dg-do compile } */
/* { dg-options "-pedantic-errors -w" } */
#include "pr36901.h"
void foo(void)
{
  int s = sc;
}
