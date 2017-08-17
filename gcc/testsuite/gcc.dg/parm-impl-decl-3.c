/* Like parm-impl-decl-1.c, but with -g.  PR 43381.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-g -std=gnu89" } */

int
foo (int __attribute__ ((__mode__ (vector_size(8)))) i) /* { dg-warning "'mode' attribute ignored" } */
{
  return (long long) i;
}

int f (int [sizeof(g())]);
int f1 (int [sizeof(g1())]);

int g () { return 1; }

int
h (int (*p)[sizeof(i())])
{
  int g2 (), g3 ();
  return (*p)[0] + g3() + g2();
}

int i () { return 2; }

int f2 (int [sizeof(g2())]);
int f3 (int [sizeof(g3())]);
int g3 () { return 4; }
