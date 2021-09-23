/* PR tree-optimization/101741 */
/* { dg-do compile } */
/* { dg-options "-O2 " } */

int
foo (void);

unsigned int
toupper (int c)
{
  c = foo ();
  while (c)
    c = toupper (c);

  return c;
}
