// PR target/41680
// { dg-do compile }

extern void baz (float);

inline bool
bar (float x)
{
  union { float f; int i; } u;
  u.f = x;
  return (u.i & 1);
}

void
foo (float *x)
{
  for (int i = 0; i < 10; i++)
    {
      float f = x[i];
      if (!bar (f))
	baz (f);
    }
}
