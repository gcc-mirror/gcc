/* { dg-do run } */

#include <stdio.h>

#include "init3.h"
#include "dump.h"

#ifdef __cplusplus
extern "C"
#endif
void abort (void);

int Get_Elem1 (struct R1 R)
{
  struct R1 Tmp = R;
  return Tmp.I;
}

void Set_Elem1 (struct R1 *R, int I)
{
  struct R1 Tmp = *R;
  Tmp.I = I;
  *R = Tmp;
}

int Get_Elem2 (struct R2 R)
{
  struct R2 Tmp = R;
  return Tmp.I;
}

void Set_Elem2 (struct R2 *R, int I)
{
  struct R2 Tmp = *R;
  Tmp.I = I;
  *R = Tmp;
}

int main (void)
{
  struct R1 A1 = My_R1;
  struct R2 A2 = My_R2;

  put ("A1 :");
  dump (&A1, sizeof (struct R1));
  new_line ();
  /* { dg-output "A1 : c2 7b f3 2a 5e 12 9a 95.*\n" } */

  put ("A2 :");
  dump (&A2, sizeof (struct R2));
  new_line ();
  /* { dg-output "A2 : 09 e2 af 37 bd 24 d2 b4.*\n" } */

  if (Get_Elem1 (A1) != 0x78ABCDEF) abort ();

  Set_Elem1 (&A1, 0xCD0034);
  if (Get_Elem1 (A1) != 0xCD0034) abort ();

  if (Get_Elem2 (A2) != 0x78ABCDEF) abort ();

  Set_Elem2 (&A2, 0xCD0034);
  if (Get_Elem2 (A2) != 0xCD0034) abort ();

  new_line ();
  return 0;
}
