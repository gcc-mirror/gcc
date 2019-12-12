/* Test old-style function definitions not in C2x: () gives type with
   a prototype.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

void
f ()
{
}

void
g (void)
{
  f (1); /* { dg-error "too many arguments to function" } */
}
