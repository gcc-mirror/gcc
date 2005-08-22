/* { dg-do compile } */
/* { dg-options "-Wswitch-enum" } */

enum a { a0, a1, a2, a3 };

int error(enum a aa)
{
  switch ( aa )
  {
  case a0 ... a3:
    return 1;
  }
  return 0;
}
