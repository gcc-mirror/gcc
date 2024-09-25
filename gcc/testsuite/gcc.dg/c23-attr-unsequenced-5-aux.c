/* Auxiliary source for c23-attr-unsequenced-5.c test.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int
f1 () [[unsequenced]]
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
