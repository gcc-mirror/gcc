/* PR tree-optimization/88739 */
#if __SIZEOF_SHORT__ == 2 &&  __SIZEOF_INT__ == 4 && __CHAR_BIT__ == 8
struct A
{
  unsigned int a, b, c;
  unsigned int d : 30;
  unsigned int e : 2;
};

union U
{
  struct A f;
  unsigned int g[4];
  unsigned short h[8];
  unsigned char i[16];
};
volatile union U v = { .f.d = 0x4089 };

__attribute__((noipa)) void
bar (int x)
{
  static int i;
  switch (i++)
    {
    case 0: if (x != v.f.d) __builtin_abort (); break;
    case 1: if (x != v.f.e) __builtin_abort (); break;
    case 2: if (x != v.g[3]) __builtin_abort (); break;
    case 3: if (x != v.h[6]) __builtin_abort (); break;
    case 4: if (x != v.h[7]) __builtin_abort (); break;
    default: __builtin_abort (); break;
    }
}

void
foo (unsigned int x)
{
  union U u;
  u.f.d = x >> 2;
  u.f.e = 0;
  bar (u.f.d);
  bar (u.f.e);
  bar (u.g[3]);
  bar (u.h[6]);
  bar (u.h[7]);
}

int
main ()
{
  foo (0x10224);
  return 0;
}
#else
int
main ()
{
  return 0;
}
#endif
