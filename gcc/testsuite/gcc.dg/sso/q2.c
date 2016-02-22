/* { dg-do run } */

#include <stdio.h>

#include "init2.h"
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
  /* { dg-output "A1 : e2 59 d1 48 b4 aa d9 bb.*\n" } */

  put ("B1 :");
  dump (&B1, sizeof (struct R1));
  new_line ();
  /* { dg-output "B1 : e2 59 d1 48 b4 aa d9 bb.*\n" } */

  put ("A2 :");
  dump (&A2, sizeof (struct R2));
  new_line ();
  /* { dg-output "A2 : 84 8d 15 9e 15 5b 35 df.*\n" } */

  put ("B2 :");
  dump (&B2, sizeof (struct R2));
  new_line ();
  /* { dg-output "B2 : 84 8d 15 9e 15 5b 35 df.*\n" } */

  if (A1.S1 != B1.S1) abort ();

  if (A1.S1 != 2) abort ();

  if (A2.S1 != B2.S1) abort ();

  if (A2.S1 != 2) abort ();

  if (A1.I != B1.I || A1.A1 != B1.A1) abort ();

  if (A2.I != B2.I || A2.A1 != B2.A1) abort ();

  new_line ();
  return 0;
}
