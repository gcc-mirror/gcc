/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */
#include "pr36901-system.h"
void foo(void)
{
  int s = sc;
}
