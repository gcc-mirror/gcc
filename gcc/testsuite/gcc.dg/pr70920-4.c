/* { dg-require-effective-target lp64 } */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop-details -Wno-int-to-pointer-cast" } */

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

/* { dg-final { scan-tree-dump "if \\(_\[0-9\]* == 0\\)" "forwprop1" } } */
