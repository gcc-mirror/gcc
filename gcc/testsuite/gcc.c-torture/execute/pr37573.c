/* PR tree-optimization/37573 */
/* { dg-require-effective-target int32plus } */

struct S
{
  unsigned int *a;
  unsigned int b;
  unsigned int c[624];
};

static unsigned char __attribute__((noinline))
foo (struct S *s)
{
  unsigned int r;
  if (!--s->b)
    {
      unsigned int *c = s->c;
      unsigned int i;
      s->a = c;
      for (i = 0; i < 227; i++)
	c[i] = ((((c[i] ^ c[i + 1]) & 0x7ffffffe) ^ c[i]) >> 1)
	    ^ ((0 - (c[i + 1] & 1)) & 0x9908b0df) ^ c[i + 397];
    }
  r = *(s->a++);
  r ^= (r >> 11);
  r ^= ((r & 0xff3a58ad) << 7);
  r ^= ((r & 0xffffdf8c) << 15);
  r ^= (r >> 18);
  return (unsigned char) (r >> 1);
}

static void __attribute__((noinline))
bar (unsigned char *p, unsigned int q, unsigned int r)
{
  struct S s;
  unsigned int i;
  unsigned int *c = s.c;
  *c = r;
  for (i = 1; i < 624; i++)
    c[i] = i + 0x6c078965 * ((c[i - 1] >> 30) ^ c[i - 1]);
  s.b = 1;
  while (q--)
    *p++ ^= foo (&s);
};

static unsigned char p[23] = {
  0xc0, 0x49, 0x17, 0x32, 0x62, 0x1e, 0x2e, 0xd5, 0x4c, 0x19, 0x28, 0x49,
  0x91, 0xe4, 0x72, 0x83, 0x91, 0x3d, 0x93, 0x83, 0xb3, 0x61, 0x38
};

static unsigned char q[23] = {
  0x3e, 0x41, 0x55, 0x54, 0x4f, 0x49, 0x54, 0x20, 0x55, 0x4e, 0x49, 0x43,
  0x4f, 0x44, 0x45, 0x20, 0x53, 0x43, 0x52, 0x49, 0x50, 0x54, 0x3c
};

int
main (void)
{
  unsigned int s;
  s = 23;
  bar (p, s, s + 0xa25e);
  if (__builtin_memcmp (p, q, s) != 0)
    __builtin_abort ();
  return 0;
}

