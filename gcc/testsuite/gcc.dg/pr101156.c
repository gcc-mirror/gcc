/* { dg-do compile } */
/* { dg-options "-fchecking" } */

struct S { int i; };
void baz(struct S *p)
{
  __builtin_setjmp(p--);
}
