/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fcf-protection=branch" } */

char *
foo (char *dest, const char *src)
{
  while ((*dest++ = *src++) != '\0')
    /* nothing */;
  return --dest;
}

/* { dg-final { scan-assembler "\t\.cfi_startproc\n\tendbr(32|64)\n" } } */
