/* { dg-options "-Wparentheses" } */

extern void f2 (int);

void bar (int x, int y, int z)
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
}
