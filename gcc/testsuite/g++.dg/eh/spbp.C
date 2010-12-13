/* { dg-do run } */
/* { dg-options "-gdwarf-2" } */
/* { dg-skip-if "No Dwarf" { { *-*-aix* alpha*-dec-osf* hppa*-*-hpux* } && { ! hppa*64*-*-* } } { "*" } { "" } } */

/* This was a bug on x86-darwin, where the register numbering for SP
   and BP was swapped (it's easy to do because on that port it's
   different for eh_frame and debug_frame).  */

#include <stdlib.h>

void f1(int t)
{
  char u[t];
  throw 1;
}

int main()
{
  bool b = true;
  try {
    f1(100);
  } catch (int x) {
    if (b)
      exit (0);
  }
  abort ();
}
