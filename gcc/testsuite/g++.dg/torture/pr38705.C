// PR c++/38705
// { dg-do compile }

typedef int T;
typedef __SIZE_TYPE__ size_t;
extern "C" void *memcpy (void *, const void *, size_t);

void
foo (char *p, const int q)
{
  memcpy (p, &q, sizeof (int));
}

struct S
{
  T t;
  int u;
  int bar () const;
  template <class T> void foo (const T &x) const {}
};

int
S::bar () const
{
  foo (u);
  foo (t);
  return 0;
}
