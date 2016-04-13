/* { dg-do run } */

#include <stdio.h>

#include "init9.h"
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
  /* { dg-output "A1 : 18 2d 44 54 fb 21 09 40.*\n" } */

  put ("B1 :");
  dump (&B1, sizeof (struct R1));
  new_line ();
  /* { dg-output "B1 : 18 2d 44 54 fb 21 09 40.*\n" } */

  put ("A2 :");
  dump (&A2, sizeof (struct R2));
  new_line ();
  /* { dg-output "A2 : 40 09 21 fb 54 44 2d 18.*\n" } */

  put ("B2 :");
  dump (&B2, sizeof (struct R2));
  new_line ();
  /* { dg-output "B2 : 40 09 21 fb 54 44 2d 18.*\n" } */

  if (A1.F != B1.F) abort ();

  if (A1.F != Pi) abort ();

  if (A2.F != B2.F) abort ();

  if (A2.F != Pi) abort ();

  new_line ();
  return 0;
}
