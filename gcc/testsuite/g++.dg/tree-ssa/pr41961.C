// { dg-do compile }
// { dg-require-effective-target pthread }
// { dg-options "-O3 -ftree-parallelize-loops=2" }

struct A
{
    char c[17];
      void foo();
};

void A::foo()
{
    for (int i = 0; i < 17; ++i)
          c[i] = 0;
}
