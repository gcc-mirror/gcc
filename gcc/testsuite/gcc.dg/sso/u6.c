/* { dg-do run } */

#include <stdio.h>

#include "init6.h"
#include "dump.h"

int main (void)
{
  struct R1 Local_R1;
  struct R2 Local_R2;
  int C1;
  int C2;

  Local_R1.I      = 1;
  Local_R1.N.A[0] = 0xAB0012;
  Local_R1.N.A[1] = 0xCD0034;
  Local_R1.N.A[2] = 0xEF0056;
  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 01 00 00 00 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" } */

  Local_R2.I      = 1;
  Local_R2.N.A[0] = 0xAB0012;
  Local_R2.N.A[1] = 0xCD0034;
  Local_R2.N.A[2] = 0xEF0056;
  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 00 00 00 01 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" } */

  C1 = Local_R1.N.A[Local_R1.I];
  printf ("C1 : %d\n", C1);
  /* { dg-output "C1 : 13434932.*\n" } */

  Local_R1.I++;
  C1 = Local_R1.N.A[Local_R1.I];
  printf ("C1 : %d\n", C1);
  /* { dg-output "C1 : 15663190.*\n" } */

  C2 = Local_R2.N.A[Local_R2.I];
  printf ("C2 : %d\n", C2);
  /* { dg-output "C2 : 13434932.*\n" } */

  Local_R2.I++;
  C2 = Local_R2.N.A[Local_R2.I];
  printf ("C2 : %d\n", C2);
  /* { dg-output "C2 : 15663190.*\n" } */

  new_line ();
  return 0;
}
