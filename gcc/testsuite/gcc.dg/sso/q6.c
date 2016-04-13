/* { dg-do run } */

#include <stdio.h>

#include "init6.h"
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
  /* { dg-output "A1 : 78 56 34 12 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" } */

  put ("B1 :");
  dump (&B1, sizeof (struct R1));
  new_line ();
  /* { dg-output "B1 : 78 56 34 12 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" } */
  put ("A2 :");
  dump (&A2, sizeof (struct R2));
  new_line ();
  /* { dg-output "A2 : 12 34 56 78 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" } */

  put ("B2 :");
  dump (&B2, sizeof (struct R2));
  new_line ();
  /* { dg-output "B2 : 12 34 56 78 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" } */

  if (A1.I != B1.I || A1.N.A[0] != B1.N.A[0]) abort();

  if (A2.I != B2.I || A2.N.A[0] != B2.N.A[0]) abort ();

  new_line ();
  return 0;
}
