/* { dg-do compile } */
/* { dg-options "-Wenum-conversion" } */

enum X { x1, x2 };
enum Y { y1, y2 };

enum X obj = y1;  /* { dg-warning "implicit conversion from .enum Y. to .enum X." } */
enum Y obj2 = y1;

enum X obj3;
void foo()
{
  obj3 = y2; /* { dg-warning "implicit conversion from .enum Y. to .enum X." } */
}

void bar(enum X);
void f(void)
{
  bar (y1); /* { dg-warning "implicit conversion from .enum Y. to .enum X." } */
}
