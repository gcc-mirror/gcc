/* { dg-do run } */

#include <stdio.h>

#include "init7.h"
#include "dump.h"

int main (void)
{
  struct R1 Local_R1;
  struct R2 Local_R2;

  Local_R1.I    = My_R1.I + 1;
  Local_R1.N.C1 = My_R1.N.C1 + 1;
  Local_R1.N.C2 = My_R1.N.C2 + 1;
  Local_R1.N.C3 = My_R1.N.C3 + 1;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 79 56 34 12 13 00 ab 00 35 00 cd 00 57 00 ef 00.*\n" } */

  Local_R2.I    = My_R2.I + 1;
  Local_R2.N.C1 = My_R2.N.C1 + 1;
  Local_R2.N.C2 = My_R2.N.C2 + 1;
  Local_R2.N.C3 = My_R2.N.C3 + 1;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 12 34 56 79 00 ab 00 13 00 cd 00 35 00 ef 00 57.*\n" } */

  Local_R1.I    = 0x12345678;
  Local_R1.N.C1 = 0xAB0012;
  Local_R1.N.C2 = 0xCD0034;
  Local_R1.N.C3 = 0xEF0056;
  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 78 56 34 12 12 00 ab 00 34 00 cd 00 56 00 ef 00.*\n" } */

  Local_R2.I    = 0x12345678;
  Local_R2.N.C1 = 0xAB0012;
  Local_R2.N.C2 = 0xCD0034;
  Local_R2.N.C3 = 0xEF0056;
  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 12 34 56 78 00 ab 00 12 00 cd 00 34 00 ef 00 56.*\n" } */

  Local_R1.I    = Local_R1.I + 1;
  Local_R1.N.C1 = Local_R1.N.C1 + 1;
  Local_R1.N.C2 = Local_R1.N.C2 + 1;
  Local_R1.N.C3 = Local_R1.N.C3 + 1;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 79 56 34 12 13 00 ab 00 35 00 cd 00 57 00 ef 00.*\n" } */

  Local_R2.I    = Local_R2.I + 1;
  Local_R2.N.C1 = Local_R2.N.C1 + 1;
  Local_R2.N.C2 = Local_R2.N.C2 + 1;
  Local_R2.N.C3 = Local_R2.N.C3 + 1;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 12 34 56 79 00 ab 00 13 00 cd 00 35 00 ef 00 57.*\n" } */

  new_line ();
  return 0;
}
