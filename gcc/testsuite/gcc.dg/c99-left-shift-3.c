/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -Wno-shift-overflow" } */

#define INTM1 (sizeof (int) * __CHAR_BIT__ - 1)

enum { A = 1 << INTM1 }; /* { dg-error "constant expression" } */
int k = 1 << INTM1; /* { dg-error "constant expression" } */

void
fn (int i)
{
  switch (i)
  case 1 << INTM1: break; /* { dg-error "constant expression" } */
}
