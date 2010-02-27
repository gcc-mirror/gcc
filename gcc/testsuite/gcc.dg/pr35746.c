/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

int foo(int i);

void bar()
{
  __complex__ int i;
  X j;			/* { dg-error "undeclared|expected" } */
                        /* { dg-message "undeclared identifier is reported only once" "reminder" { target *-*-* } 9 } */
  if (i = foo(j))	/* { dg-error "undeclared" } */
    ;
}
