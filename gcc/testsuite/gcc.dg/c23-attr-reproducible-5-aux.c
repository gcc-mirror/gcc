/* Auxiliary source for c23-attr-reproducible-5.c test.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int
f1 () [[reproducible]]
{
  return 42;
}

int
f2 ()
{
  return 43;
}

int
f3 ()
{
  return 44;
}
