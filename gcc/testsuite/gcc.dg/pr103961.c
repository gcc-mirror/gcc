/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void abort ();

extern inline __attribute__ ((__gnu_inline__)) int
sprintf (char *restrict s, const char *restrict fmt, ...)
{
  return __builtin___sprintf_chk (s, 1, __builtin_object_size (s, 1),
				  fmt, __builtin_va_arg_pack ());
}

void
cap_to_text (int c)
{
  char buf[1572];
  char *p;
  int n, t;
  p = 20 + buf;
  for (t = 8; t--; )
    {
      for (n = 0; n < c; n++)
	p += sprintf (p, "a,");
      p--;
      if (__builtin_object_size (p, 1) == 0)
	abort ();
    }
}

/* { dg-final { scan-assembler-not "abort" } } */
