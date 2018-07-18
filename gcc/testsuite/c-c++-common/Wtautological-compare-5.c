/* PR c/81783 */
/* { dg-do compile } */
/* { dg-options "-Wtautological-compare" } */

enum E { FOO = 128 };

int
f (int a)
{
  if ((a & 16) == 10) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if ((16 & a) == 10) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if (10 == (a & 16)) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if (10 == (16 & a)) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;

  if ((a & 16) != 10) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if ((16 & a) != 10) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if (10 != (a & 16)) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if (10 != (16 & a)) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;

  if ((a & 9) == 8)
    return 1;
  if ((9 & a) == 8)
    return 1;
  if (8 == (a & 9))
    return 1;
  if (8 == (9 & a))
    return 1;

  if ((a & 9) != 8)
    return 1;
  if ((9 & a) != 8)
    return 1;
  if (8 != (a & 9))
    return 1;
  if (8 != (9 & a))
    return 1;

  if ((a | 16) == 10) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if ((16 | a) == 10) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if (10 == (a | 16)) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if (10 == (16 | a)) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;

  if ((a | 16) != 10) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if ((16 | a) != 10) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if (10 != (a | 16)) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if (10 != (16 | a)) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;

  if ((a | 9) == 8) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if ((9 | a) == 8) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if (8 == (a | 9)) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if (8 == (9 | a)) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;

  if ((a | 9) != 8) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if ((9 | a) != 8) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if (8 != (a | 9)) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if (8 != (9 | a)) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;

  if ((a & 128) != 1) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if ((128 & a) != 1) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if ((a & FOO) != 1) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if ((FOO & a) != 1) /* { dg-warning "bitwise comparison always evaluates to true" } */
    return 1;
  if ((a & 128) == 1) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if ((128 & a) == 1) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if ((a & FOO) == 1) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;
  if ((FOO & a) == 1) /* { dg-warning "bitwise comparison always evaluates to false" } */
    return 1;

#define N 0x10
  if ((a & N) == 10) /* { dg-bogus "bitwise comparison always evaluates to false" "" { xfail *-*-* } } */
    return 1;
  if ((a | N) == 10) /* { dg-bogus "bitwise comparison always evaluates to false" "" { xfail *-*-* } } */
    return 1;

  return 0;
}
