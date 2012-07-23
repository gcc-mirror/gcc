/* pr36134.c

   This test ensures that the shorter LEA instruction is used in preference
   to the longer ADD instruction.

   This preference is applicable to ColdFire only.  On CPU32, we can
   use a sequence of two ADDQ instructions, which is faster than the
   LEA instruction.  */

/* { dg-do compile }  */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" } { "-mcpu=5208" } } */
/* { dg-options "-O2 -mcpu=5208" }  */
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
