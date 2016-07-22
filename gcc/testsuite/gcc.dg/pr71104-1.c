/* { dg-do compile } */

void foo(void);
int vfork(void);
int *p;

void bar(void)
{
  foo();
  *p = vfork();
}
