/* PR c/71573 */
/* { dg-do compile } */

void
f1 (void)
{
  extern int t;
}

void
f2 (void)
{
  t (0); /* { dg-error "called object .t. is not a function or function pointer" } */
}
