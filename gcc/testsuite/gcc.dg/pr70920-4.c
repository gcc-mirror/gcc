/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fdump-tree-ccp1 -Wno-int-to-pointer-cast" } */

#include <stdint.h>

void f1();
void f2();

void
foo (int a)
{
  void *cst = 0; 
  if ((int *) a == cst)
    {
      f1 ();
      if (a) 
	f2 (); 
    }
}

/* { dg-final { scan-tree-dump "if \\(a_\[0-9\]*\\(D\\) == 0\\)" "ccp1" } } */
