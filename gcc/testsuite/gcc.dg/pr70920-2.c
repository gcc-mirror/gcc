/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop-details" } */

#include <stdint.h>

void f1();
void f2();

void
foo (int *a)
{
  int cst = 0;
  if ((intptr_t) a == cst)
    {
      f1 ();
      if (a) 
	f2 (); 
    }
}

/* { dg-final { scan-tree-dump "gimple_simplified to if \\(a_\[0-9\]*\\(D\\) == 0B\\)" "forwprop1" } } */
