/* { dg-do compile } */
/* { dg-options "-O" } */

struct S
{
  int i;
};

int invalid[] = 0; /* { dg-error "invalid initializer" } */

void foo (void)
{
  if (((struct S *)undeclared)->i); /* { dg-error "undeclared" } */
  /* { dg-message "reported only once" "" { target *-*-* } .-1 } */
}

