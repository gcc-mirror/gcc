/* { dg-do compile } */
/* { dg-options "-Walloca -O2" } */

// Make sure we don't warn on VLA with -Walloca.

void f (void*);

void h1 (unsigned n)
{
  int a [n];
  f (a);
}
