/* Test -feliminate-dwarf2-dups */
/* Contributed by Devang Patel <dpatel@apple.com> */
/* { dg-do compile } */
/* { dg-options "-feliminate-dwarf2-dups" } */

#include "dwarf2-3.h"

int main()
{
  struct point p;
  p.x = 0;
  p.y = 0;
}
