/* { dg-do run } */

#include <stdio.h>

#include "init3.h"
#include "dump.h"

int main (void)
{
  struct R1 Local_R1;
  struct R2 Local_R2;

  Local_R1.S1   = My_R1.S1 - 1;
  Local_R1.I    = My_R1.I + 1;
  Local_R1.S2   = My_R1.S2 - 1;
  Local_R1.N.C1 = My_R1.N.C1 % 16;
  Local_R1.N.C2 = My_R1.N.C2 % 16;
  Local_R1.N.C3 = My_R1.N.C3 % 16;
  Local_R1.N.B  = My_R1.N.B % 2;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 01 7c f3 2a 1e 02 82 01.*\n" } */

  Local_R2.S1   = My_R2.S1 - 1;
  Local_R2.I    = My_R2.I + 1;
  Local_R2.S2   = My_R2.S2 - 1;
  Local_R2.N.C1 = My_R2.N.C1 % 16;
  Local_R2.N.C2 = My_R2.N.C2 % 16;
  Local_R2.N.C3 = My_R2.N.C3 % 16;
  Local_R2.N.B  = My_R2.N.B % 2;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 05 e2 af 37 c0 04 10 30.*\n" } */

  Local_R1.S1   = 2;
  Local_R1.I    = 0x78ABCDEF;
  Local_R1.S2   = 1;
  Local_R1.N.C1 = 0x12;
  Local_R1.N.C2 = 0x34;
  Local_R1.N.C3 = 0x56;
  Local_R1.N.B  = 4;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : c2 7b f3 2a 5e 12 9a 95.*\n" } */

  Local_R2.S1   = 2;
  Local_R2.I    = 0x78ABCDEF;
  Local_R2.S2   = 1;
  Local_R2.N.C1 = 0x12;
  Local_R2.N.C2 = 0x34;
  Local_R2.N.C3 = 0x56;
  Local_R2.N.B  = 4;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 09 e2 af 37 bd 24 d2 b4.*\n" } */

  Local_R1.S1   = Local_R1.S1 - 1;
  Local_R1.I    = Local_R1.I + 1;
  Local_R1.S2   = Local_R1.S2 - 1;
  Local_R1.N.C1 = Local_R1.N.C1 % 16;
  Local_R1.N.C2 = Local_R1.N.C2 % 16;
  Local_R1.N.C3 = Local_R1.N.C3 % 16;
  Local_R1.N.B  = Local_R1.N.B % 2;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 01 7c f3 2a 1e 02 82 01.*\n" } */

  Local_R2.S1   = Local_R2.S1 - 1;
  Local_R2.I    = Local_R2.I + 1;
  Local_R2.S2   = Local_R2.S2 - 1;
  Local_R2.N.C1 = Local_R2.N.C1 % 16;
  Local_R2.N.C2 = Local_R2.N.C2 % 16;
  Local_R2.N.C3 = Local_R2.N.C3 % 16;
  Local_R2.N.B  = Local_R2.N.B % 2;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 05 e2 af 37 c0 04 10 30.*\n" } */

  new_line ();
  return 0;
}
