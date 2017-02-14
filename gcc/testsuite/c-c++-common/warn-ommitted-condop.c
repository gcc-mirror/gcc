/* { dg-options "-Wparentheses -ftrack-macro-expansion=0" } */

#ifndef __cplusplus
#define bool _Bool
#endif

extern void f2 (int);

void bar (int x, int y, int z, bool b)
{
#define T(op) f2 (x op y ? : 1)
#define T2(op) f2 (x op y ? 2 : 1)

  T(<); /* { dg-warning "omitted middle operand" } */
  T(>); /* { dg-warning "omitted middle operand" } */
  T(<=); /* { dg-warning "omitted middle operand" } */
  T(>=); /* { dg-warning "omitted middle operand" } */
  T(==); /* { dg-warning "omitted middle operand" } */
  T(!=); /* { dg-warning "omitted middle operand" } */
  T(||); /* { dg-warning "omitted middle operand" } */
  T(&&); /* { dg-warning "omitted middle operand" } */
  f2 (!x ? : 1);  /* { dg-warning "omitted middle operand" } */
  f2 ((x,!x) ? : 1);  /* { dg-warning "omitted middle operand" } */
  f2 ((x,y,!x) ? : 1);  /* { dg-warning "omitted middle operand" } */
  T2(<); /* { dg-bogus "omitted middle operand" } */
  T2(>); /* { dg-bogus "omitted middle operand" } */
  T2(==); /* { dg-bogus "omitted middle operand" } */
  T2(||); /* { dg-bogus "omitted middle operand" } */
  T2(&&); /* { dg-bogus "omitted middle operand" } */
  T(+); /* { dg-bogus "omitted middle operand" } */
  T(-); /* { dg-bogus "omitted middle operand" } */
  T(*); /* { dg-bogus "omitted middle operand" } */
  T(/); /* { dg-bogus "omitted middle operand" } */
  T(^); /* { dg-bogus "omitted middle operand" } */
  f2 (b ? : 1);  /* { dg-warning "omitted middle operand" } */
}
