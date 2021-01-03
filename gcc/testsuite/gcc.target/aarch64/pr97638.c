/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=bti" } */

char *foo (const char *s, const int c)
{
  const char *p = 0;
  for (;;)
  {
        if (*s == c)
            p = s;
        if (p != 0 || *s++ == 0)
            break;
  }
  return (char *)p;
}

/* { dg-final { scan-assembler "hint\t34" } } */
