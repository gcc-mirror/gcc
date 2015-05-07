/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

enum E { A = -2 << 1 };
int i = -1 << 0;

int
f (int i)
{
  switch (i)
  case -1 << 0: break;
}
