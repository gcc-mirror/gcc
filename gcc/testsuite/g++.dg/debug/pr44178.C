// PR debug/44178
// { dg-do compile }
// { dg-options "-funroll-loops -fcompare-debug" { target i?86-*-* x86_64-*-* } }
// { dg-options "-fsched-pressure -funroll-loops -fschedule-insns -fcompare-debug" { target i?86-*-* x86_64-*-* } }

struct A
{
  A ();
  A (const A &) {}
  A &operator = (const A &);
};

struct B
{
  int u1;
  A u2;
  int u3;
  int i;
};

B f1 (int *);
B f2 (int, int, int, int);
B f3 (B *, B *);

B
f4 (int x, int y, int z)
{
  B b1, b2;
  for (int i = x; i > 0; i--)
    for (int j = y; j > 0; j--)
      {
	int k;
	f1 (&k);
	b2 = f2 (i, 0, 0, z);
	if (b2.i) return b2;
	f3 (&b1, &b2);
      }
  return b1;
}
