/* { dg-do compile } */
/* { dg-options "-O2" } */

struct A
{
  int a1;
  int a2;
};

extern __thread const unsigned char *tcc1, **tcc2;

extern inline const unsigned char ** __attribute__ ((const))
foo (void)
{
  const unsigned char **a = &tcc1;
  if (*a == 0)
    *a = *tcc2 + 128;
  return a;
}

extern inline int
bar (const struct A *x)
{
  int a;

  if (x->a2 & 8)
    return 0;
  a = x->a1;
  return a > 0 && ((*foo ())[a] & 64);
}

int
baz (const struct A *x, char *y)
{
  const struct A *a;

  for (a = x; !!a->a1; a++)
    if (! (x->a2 & 8))
      if (bar (a))
	{
	  *y++ = a->a1;
	  if (x->a1)
	    *y++ = ':';
	  *y = '\0';
	}
  return 0;
}

/* Verify tcc1 and tcc2 variables show up only in the TLS access sequences.  */
/* { dg-final { scan-assembler "tcc1@" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler "tcc2@" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler-not "tcc1\[^@\]" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler-not "tcc2\[^@\]" { target i?86-*-* x86_64-*-* } } } */
