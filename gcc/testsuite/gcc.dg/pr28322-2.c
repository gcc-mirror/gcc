/* PR28322: ignore unknown -Wno-* if no warning is emitted.  */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra -Wno-foobar" } */

int foo (void) 
{
  int i = 1/0;  /* { dg-warning "division by zero" } */
  return i;
}

/* { dg-warning "unrecognized command-line option .-Wno-foobar." "" { target *-*-* } 0 } */

