/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple -Wno-int-to-pointer-cast" } */

#include <stdint.h>

void f1();
void f2();

void
foo (int a)
{
  if ((int *) a == 0)
    {
      f1 ();
      if (a)
	f2 (); 
    }
}

/* { dg-final { scan-tree-dump "if \\(a == 0\\)" "gimple" } } */
