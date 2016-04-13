/* { dg-do run } */

#include <stdio.h>

#include "init2.h"
#include "dump.h"

int main (void)
{
  struct R1 Local_R1;
  struct R2 Local_R2;

  put ("My_R1    :");
  dump (&My_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "My_R1    : e2 59 d1 48 b4 aa d9 bb.*\n" } */

  put ("My_R2    :");
  dump (&My_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "My_R2    : 84 8d 15 9e 15 5b 35 df.*\n" } */

  Local_R1 = My_R1;
  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : e2 59 d1 48 b4 aa d9 bb.*\n" } */

  Local_R2 = My_R2;
  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 84 8d 15 9e 15 5b 35 df.*\n" } */

  Local_R1.S1 = 2;
  Local_R1.I  = 0x12345678;
  Local_R1.S2 = 1;
  Local_R1.A1 = 0xAB;
  Local_R1.A2 = 0xCD;
  Local_R1.A3 = 0xEF;
  Local_R1.B  = 1;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : e2 59 d1 48 b4 aa d9 bb.*\n" } */

  Local_R2.S1 = 2;
  Local_R2.I  = 0x12345678;
  Local_R2.S2 = 1;
  Local_R2.A1 = 0xAB;
  Local_R2.A2 = 0xCD;
  Local_R2.A3 = 0xEF;
  Local_R2.B  = 1;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 84 8d 15 9e 15 5b 35 df.*\n" } */

  Local_R1.S1 = Local_R2.S1;
  Local_R1.I  = Local_R2.I;
  Local_R1.S2 = Local_R2.S2;
  Local_R1.A1 = Local_R2.A1;
  Local_R1.A2 = Local_R2.A2;
  Local_R1.A3 = Local_R2.A3;
  Local_R1.B  = Local_R2.B;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : e2 59 d1 48 b4 aa d9 bb.*\n" } */

  Local_R2.S1 = Local_R1.S1;
  Local_R2.I  = Local_R1.I;
  Local_R2.S2 = Local_R1.S2;
  Local_R2.A1 = Local_R1.A1;
  Local_R2.A2 = Local_R1.A2;
  Local_R2.A3 = Local_R1.A3;
  Local_R2.B  = Local_R1.B;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 84 8d 15 9e 15 5b 35 df.*\n" } */

  new_line ();
  return 0;
}
