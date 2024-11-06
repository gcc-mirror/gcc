/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

void foo();

void bar(int i)
{
  foo (i);
}

void foo(int *p)
{
  *p = 0;
}
