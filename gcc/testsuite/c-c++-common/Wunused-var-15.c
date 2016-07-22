/* PR c/71719 */
/* { dg-do compile } */
/* { dg-options "-Wunused -W -Wno-psabi" } */

typedef unsigned V __attribute__ ((vector_size (16)));

void bar (unsigned);

V x;

void
foo (V v)	/* { dg-bogus "set but not used" } */
{
  bar (v[0]);
  V w = x;	/* { dg-bogus "set but not used" } */
  bar (w[1]);
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector passed by reference.*" } */
