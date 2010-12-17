/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

int foo(int i);

void bar()
{
  __complex__ int i;
  X j;			/* { dg-error "unknown" } */
  if (i = foo(j))
    ;
}
