/* { dg-do compile } */
/* { dg-options "-O --param=tree-reassoc-width=256" } */

unsigned a, b;

void
foo (unsigned c)
{
  a += b + c + 1;
}
