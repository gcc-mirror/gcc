/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -Wno-shift-overflow" } */

#define INTM2 (sizeof (int) * __CHAR_BIT__ - 2)

enum { A = 10 << INTM2 }; /* { dg-error "constant expression" } */
int k = 10 << INTM2; /* { dg-error "constant expression" } */

void
fn (int i)
{
  switch (i)
  case 10 << INTM2: break; /* { dg-error "constant expression" } */
}
