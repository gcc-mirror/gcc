// { dg-do compile }
// { dg-require-effective-target pthread }
// { dg-options "-ftree-parallelize-loops=2 -g" }

struct A
{
  int zero ()
  {
    return 0;
  }
};

static inline void
bar (int)
{
}

struct B
{
  struct A a;
  B (int n)
  {
    for (int i = 0; i < n; i++)
      bar (a.zero ());
  }
};

void
foo (int n)
{
  struct B b (n);
}
