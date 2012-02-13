/* PR rtl-optimization/51933 */

static signed char v1;
static unsigned char v2[256], v3[256];

__attribute__((noclone, noinline)) void
foo (void)
{
#if defined(__s390__) && !defined(__zarch__)
  /* S/390 31 bit cannot deal with more than one literal pool
     reference per insn.  */
  asm volatile ("" : : "g" (&v1) : "memory");
  asm volatile ("" : : "g" (&v2[0]));
  asm volatile ("" : : "g" (&v3[0]));
#else
  asm volatile ("" : : "g" (&v1), "g" (&v2[0]), "g" (&v3[0]) : "memory");
#endif
}

__attribute__((noclone, noinline)) int
bar (const int x, const unsigned short *y, char *z)
{
  int i;
  unsigned short u;
  if (!v1)
    foo ();
  for (i = 0; i < x; i++)
    {
      u = y[i];
      z[i] = u < 0x0100 ? v2[u] : v3[u & 0xff];
    }
  z[x] = '\0';
  return x;
}

int
main (void)
{
  char buf[18];
  unsigned short s[18];
  unsigned char c[18] = "abcdefghijklmnopq";
  int i;
  for (i = 0; i < 256; i++)
    {
      v2[i] = i;
      v3[i] = i + 1;
    }
  for (i = 0; i < 18; i++)
    s[i] = c[i];
  s[5] |= 0x600;
  s[6] |= 0x500;
  s[11] |= 0x2000;
  s[15] |= 0x500;
  foo ();
  if (bar (17, s, buf) != 17
      || __builtin_memcmp (buf, "abcdeghhijkmmnoqq", 18) != 0)
    __builtin_abort ();
  return 0;
}
