/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

extern void foo(void *here);
extern inline void bar(void)
{
  __label__ here;
  foo(&&here);
here:
  ;
}

void baz(void)
{
  bar();
}
