/* Test old-style function definitions not in C23: () does not give
   type with a prototype for older standards.  */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

void
f ()
{
}

void
g (void)
{
  f (1);
}
