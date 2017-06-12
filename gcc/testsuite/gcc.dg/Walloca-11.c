/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca -O2" } */

// Make sure we don't warn on VLA with -Walloca.

void f (void*);

void h1 (unsigned n)
{
  int a [n];
  f (a);
}
