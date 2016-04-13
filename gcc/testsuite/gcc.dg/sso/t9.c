/* { dg-do run } */

#include <stdio.h>

#include "init9.h"
#include "dump.h"

int main (void)
{
  struct R1 Local_R1;
  struct R2 Local_R2;

  Local_R1.F = My_R1.F + 1.0;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 8c 16 22 aa fd 90 10 40.*\n" } */

  Local_R2.F = My_R2.F + 1.0;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 40 10 90 fd aa 22 16 8c.*\n" } */

  Local_R1.F = Pi;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 18 2d 44 54 fb 21 09 40.*\n" } */

  Local_R2.F = Pi;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 40 09 21 fb 54 44 2d 18.*\n" } */

  Local_R1.F = Local_R1.F + 1.0;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 8c 16 22 aa fd 90 10 40.*\n" } */

  Local_R2.F = Local_R2.F + 1.0;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 40 10 90 fd aa 22 16 8c.*\n" } */

  new_line ();
  return 0;
}
