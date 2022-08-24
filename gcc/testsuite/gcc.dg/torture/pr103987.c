/* { dg-do compile } */

void foo();

void bar(int i)
{
  foo (i);
}

void foo(int *p)
{
  *p = 0;
}
