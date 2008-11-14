/* pr36134.c

   This test ensures that the shorter LEA instruction is used in preference
   to the longer ADD instruction.  */

/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler "lea" } } */
/* { dg-final { scan-assembler-not "add" } } */

int *a, *b;

void
f ()
{
  while (a > b)
    {
      *a++ = *b++;
      *a++ = *b++;
      *a++ = *b++;
      *a++ = *b++;
    }
}
