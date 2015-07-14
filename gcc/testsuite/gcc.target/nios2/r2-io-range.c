/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mbypass-cache" } */

/* Check that the compiler is aware of the reduced offset range for ldio/stio
   instructions in the Nios II R2 encoding.  */

unsigned int too_big (unsigned int *p)
{
  return *(p + 0x400);
}

unsigned int small_enough (unsigned int *p)
{
  return *(p + 0x100);
}

/* { dg-final { scan-assembler-not "\tldwio\t.*, 4096\\(r.*\\)" } }  */
/* { dg-final { scan-assembler "\tldwio\t.*, 1024\\(r.*\\)" } }  */
