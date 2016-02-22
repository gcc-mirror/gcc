/* { dg-do run } */

#include <stdio.h>

#include "init1.h"
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
  /* { dg-output "A1 : 78 56 34 12.*\n" } */

  put ("B1 :");
  dump (&B1, sizeof (struct R1));
  new_line ();
  /* { dg-output "B1 : 78 56 34 12.*\n" } */

  put ("A2 :");
  dump (&A2, sizeof (struct R2));
  new_line ();
  /* { dg-output "A2 : 12 34 56 78.*\n" } */

  put ("B2 :");
  dump (&B2, sizeof (struct R2));
  new_line ();
  /* { dg-output "B2 : 12 34 56 78.*\n" } */

  if (A1.I != B1.I) abort ();

  if (A1.I != 0x12345678) abort ();

  if (A2.I != B2.I) abort ();

  if (A2.I != 0x12345678) abort ();

  new_line ();
  return 0;
}
