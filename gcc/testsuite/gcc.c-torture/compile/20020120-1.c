/* This ICEed on IA-32 with -O2 -mcpu=i386, because reload was trying
   to reload into %sil register.  */

struct A
{
  void *a;
  unsigned int b, c, d;
};

struct B
{
  struct A *e;
};

void bar (struct A *);
void baz (struct A *);

static inline unsigned int
inl (unsigned int v, unsigned char w, unsigned char x, unsigned char y,
     unsigned char z)
{
  switch (v)
    {
    case 2:
      return ((w & 0xf8) << 8) | ((x & 0xfc) << 3) | ((y & 0xf8) >> 3);
    case 4:
      return (z << 24) | (w << 16) | (x << 8) | y;
    default:
      return 0;
    }
}

void foo (struct B *x, int y, const float *z)
{
  struct A *a = x->e;

  if (y)
    {
      if (x->e->a)
       bar (x->e);
    }
  else
    {
      unsigned char c[4];
      unsigned int b;

      c[0] = z[0]; c[1] = z[1]; c[2] = z[2]; c[3] = z[3];
      b = inl (a->b, c[0], c[1], c[2], c[3] );
      if (a->a)
       bar (a);
      else
       baz (a);
      a->c = b;
   }
}
