/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

enum E { A = -2 << 1 }; /* { dg-error "constant expression" } */
int i = -1 << 0; /* { dg-error "constant expression" } */

int
f (int i)
{
  switch (i)
  case -1 << 0: break; /* { dg-error "constant expression" } */
}
