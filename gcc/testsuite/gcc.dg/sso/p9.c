/* { dg-do run } */

#include <stdio.h>

#include "init9.h"
#include "dump.h"

int main (void)
{
  struct R1 Local_R1;
  struct R2 Local_R2;

  put ("My_R1    :");
  dump (&My_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "My_R1    : 18 2d 44 54 fb 21 09 40.*\n" } */

  put ("My_R2    :");
  dump (&My_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "My_R2    : 40 09 21 fb 54 44 2d 18.*\n" } */

  Local_R1 = My_R1;
  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 18 2d 44 54 fb 21 09 40.*\n" } */

  Local_R2 = My_R2;
  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 40 09 21 fb 54 44 2d 18.*\n" } */

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

  Local_R1.F = Local_R2.F;

  put ("Local_R1 :");
  dump (&Local_R1, sizeof (struct R1));
  new_line ();
  /* { dg-output "Local_R1 : 18 2d 44 54 fb 21 09 40.*\n" } */

  Local_R2.F = Local_R1.F;

  put ("Local_R2 :");
  dump (&Local_R2, sizeof (struct R2));
  new_line ();
  /* { dg-output "Local_R2 : 40 09 21 fb 54 44 2d 18.*\n" } */

  new_line ();
  return 0;
}
