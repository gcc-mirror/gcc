// PR c++/19263
// { dg-do run }
// { dg-options "-O2" }

typedef signed char v8qi __attribute__ ((vector_size (8)));

extern "C" void abort (void);

static unsigned char S[16];

struct A
{
  int i;
  v8qi j, k;
  int l;
};

void
foo (unsigned char v)
{
  A a = { 1, { v, v, v, v, v, v, v, v },
	  { v + 1, v + 1, v + 1, v + 1, v + 1, v + 1, v + 1, v + 1 }, 3 };
  v8qi *s = (v8qi *) &S[0];
  *s = a.j;
  s[1] = a.k;
}

void
bar (unsigned char v)
{
  v8qi val8 = { v, v, v, v, v, v, v, v };
  v8qi *s = (v8qi *) &S[0];
  *s = val8;
}

int n = 5, cnt;

int
num (void)
{
  ++cnt;
  return n;
}

void
baz (void)
{
  static A a = { 0, { num (), num (), num (), num (), 6, 6, 6, 6 },
		 { 7, 7, 7, 7, 8, 8, 8, 8 }, 0 };
  v8qi *s = (v8qi *) &S[0];
  *s = a.j;
  s[1] = a.k;
}

int
main ()
{
  int i;
  foo (1);
  for (i = 0; i < 8; ++i)
    if (S[i] != 1)
      abort ();
  for (; i < 16; ++i)
    if (S[i] != 2)
      abort ();
  bar (3);
  for (i = 0; i < 8; ++i)
    if (S[i] != 3)
      abort ();
  return 0;
  baz ();
  if (cnt != 4)
    abort ();
  for (i = 0; i < 16; ++i)
    if (S[i] != 5 + (i / 4))
      abort ();
  return 0;
}
