/* { dg-do run } */
/* { dg-options "-O" } */

#pragma pack(1)
struct S0
{
#if __SIZEOF_INT >= 4
  int f0:24;
#else
  __INT32_TYPE__ f0:24;
#endif
};

struct S1
{
  int f1;
} a;

int b, c;

char
fn1 (struct S1 p1)
{
  return 0;
}

int
main ()
{
  c = fn1 (a);
  if (b)
    {
      struct S0 f[3][9] =
	{ { { 0 }, { 0 }, { 1 }, { 1 }, { 0 }, { 0 }, { 0 }, { 1 }, { 1 } },
	  { { 0 }, { 0 }, { 1 }, { 1 }, { 0 }, { 0 }, { 0 }, { 1 }, { 1 } },
	  { { 0 }, { 0 }, { 1 }, { 1 }, { 0 }, { 0 }, { 0 }, { 1 }, { 1 } }
	};
      b = f[1][8].f0;
    }
  struct S0 g[3][9] =
	{ { { 0 }, { 0 }, { 1 }, { 1 }, { 0 }, { 0 }, { 0 }, { 1 }, { 1 } },
	  { { 0 }, { 0 }, { 1 }, { 1 }, { 0 }, { 0 }, { 0 }, { 1 }, { 1 } },
	  { { 0 }, { 0 }, { 1 }, { 1 }, { 0 }, { 0 }, { 0 }, { 1 }, { 1 } }
	};

  if (g[1][8].f0 != 1)
    __builtin_abort ();

  return 0;
}
