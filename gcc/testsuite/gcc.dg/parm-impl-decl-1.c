/* Test implicit function declarations and other odd declarations in
   function prototypes.  Bug 18239.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

/* Implicit function declaration in attribute in definition (testcase
   from bug).  */
int
foo (int __attribute__ ((__mode__ (vector_size(8)))) i) /* { dg-warning "warning: '__mode__' attribute ignored" } */
{
  return (long long) i;
}

/* Various other cases.  */

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
