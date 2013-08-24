// PR tree-optimization/58006
// { dg-do compile }
// { dg-require-effective-target pthread }
// { dg-options "-Ofast -ftree-parallelize-loops=2" }

extern "C" float sqrtf (float);

struct S
{
  float i, j;
  float foo () const { return sqrtf (i * i + j * j); }
  S () : i (1), j (1) {}
};

void
bar (int a, int b)
{
  int i;
  float f;
  for (i = a; i < b; i++)
    f = S ().foo ();
}
