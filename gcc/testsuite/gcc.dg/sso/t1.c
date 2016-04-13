/* { dg-do run } */

#include <stdio.h>

#include "init1.h"
#include "dump.h"

int main (void)
{
  struct R1 Local_R1;
  struct R2 Local_R2;

  Local_R1.I = My_R1.I + 1;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 79 56 34 12.*\n" } */

  Local_R2.I = My_R2.I + 1;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 12 34 56 79.*\n" } */

  Local_R1.I = 0x12345678;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 78 56 34 12.*\n" } */

  Local_R2.I = 0x12345678;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 12 34 56 78.*\n" } */

  Local_R1.I = Local_R1.I + 1;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 79 56 34 12.*\n" } */

  Local_R2.I = Local_R2.I + 1;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 12 34 56 79.*\n" } */

  new_line ();
  return 0;
}
