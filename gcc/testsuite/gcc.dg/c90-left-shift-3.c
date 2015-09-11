/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

#define INTM1 (sizeof (int) * __CHAR_BIT__ - 1)

enum { A = 1 << INTM1 };
int k = 1 << INTM1;

void
fn (int i)
{
  switch (i)
  case 1 << INTM1: break;
}
