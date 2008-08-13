/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

int foo(int i);

void bar()
{
  __complex__ int i;
  X j;			/* { dg-error "undeclared.*appears.*expected" } */

  if (i = foo(j))	/* { dg-error "undeclared" } */
    ;
}
