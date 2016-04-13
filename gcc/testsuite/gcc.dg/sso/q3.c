/* { dg-do run } */

#include <stdio.h>

#include "init3.h"
#include "dump.h"

#ifdef __cplusplus
extern "C"
#endif
void abort (void);

int main (void)
{
  struct R1 A1 = My_R1;
  struct R1 B1 = My_R1;

  struct R2 A2 = My_R2;
  struct R2 B2 = My_R2;

  put ("A1 :");
  dump (&A1, sizeof (struct R1));
  new_line ();
  /* { dg-output "A1 : c2 7b f3 2a 5e 12 9a 95.*\n" } */

  put ("B1 :");
  dump (&B1, sizeof (struct R1));
  new_line ();
  /* { dg-output "B1 : c2 7b f3 2a 5e 12 9a 95.*\n" } */

  put ("A2 :");
  dump (&A2, sizeof (struct R2));
  new_line ();
  /* { dg-output "A2 : 09 e2 af 37 bd 24 d2 b4.*\n" } */

  put ("B2 :");
  dump (&B2, sizeof (struct R2));
  new_line ();
  /* { dg-output "B2 : 09 e2 af 37 bd 24 d2 b4.*\n" } */

  if (A1.S1 != B1.S1) abort ();

  if (A1.S1 != 2) abort ();

  if (A2.S1 != B2.S1) abort ();

  if (A2.S1 != 2) abort ();

  if (A1.I != B1.I || A1.N.C1 != B1.N.C1) abort ();

  if (A2.I != B2.I || A2.N.C1 != B2.N.C1) abort ();

  new_line ();
  return 0;
}
