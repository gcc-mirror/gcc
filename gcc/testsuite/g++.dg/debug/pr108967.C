// PR debug/108967
// { dg-do compile }

struct F { unsigned short r[8]; };
extern void foo (F);

static inline F
bar (F a, F b)
{
  for (int i = 0; i < 8; ++i)
    a.r[i] = a.r[i] + b.r[i] < (unsigned short) -1 ? a.r[i] + b.r[i] : (unsigned short) -1;
  return a;
}

static inline void
baz (F v)
{
  foo (v);
}

void
qux (F a, F b)
{
  F c = bar (a, b);
  baz (c);
}

static inline F
corge (F a, F b)
{
  for (int i = 0; i < 8; ++i)
    a.r[i] = a.r[i] - b.r[i] > 0 ? a.r[i] - b.r[i] : 0;
  return a;
}

void
garply (F a, F b)
{
  F c = corge (a, b);
  baz (c);
}
