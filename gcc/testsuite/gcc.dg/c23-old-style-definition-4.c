/* Test old-style function definitions not in C23: () gives type with
   a prototype.  */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

void
f ()
{
}

void
g (void)
{
  f (1); /* { dg-error "too many arguments to function" } */
}
