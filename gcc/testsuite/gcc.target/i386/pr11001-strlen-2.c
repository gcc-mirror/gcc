/* Ensure that we don't use 'repnz scasb' in the presence of register globals.  */
/* { dg-do compile } */
/* { dg-options "-O1 -w" } */

extern __SIZE_TYPE__ strlen (const char *);
extern void *malloc (__SIZE_TYPE__);

register int regvar asm("%eax");

char *
do_copy (char *str)
{
  return malloc (strlen (str) + 1);
}

/* { dg-final { scan-assembler-not "repnz scasb" } } */
