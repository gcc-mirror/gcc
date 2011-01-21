// PR tree-optimization/47355
// { dg-do compile }
// { dg-options "-O -fipa-cp -fipa-cp-clone" }

struct T
{
  T ();
  void *p;
  ~T ();
};

void foo (T *i);

T *bar ();
void baz (T *);

struct V
{
  long q;
  T *r;
  ~V ()
  {
    while (q)
      {
	foo (r);
	++r;
	--q;
      }
    baz (r);
  }
};

void
foo ()
{
  V v;
  T t;
  v.r = bar ();
}
