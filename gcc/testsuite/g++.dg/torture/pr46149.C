// { dg-do run }
// { dg-options "-fno-tree-sra" }

struct S
{
  S ():p ((char *) __builtin_calloc (1, 1))
  {
  }
  char *p;
};

template < class T > struct A
{
  A (const S & __m1, const T & __m2):m1 (__m1), m2 (__m2)
  {
  }
  const S & m1;
  const T & m2;
};

struct B:A < S >
{
  B (const S & __v):A < S > (__v, __v)
  {
  }
};

struct C:A < B >
{
  C (const S & __e1, const B & __e2):A < B > (__e1, __e2)
  {
  }
};

struct D
{
  D (const C & __c):c (__c)
  {
  }
  const C c;
};

int
main ()
{
  S s;
  B b (s);
  C c (s, b);
  D d (c);
  return d.c.m2.m2.p[0];
}
