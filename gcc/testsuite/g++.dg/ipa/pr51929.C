// PR middle-end/51929
// { dg-do compile }
// { dg-options "-O -fno-guess-branch-probability -fipa-cp -fipa-cp-clone --param=max-inline-insns-single=25" }

struct A
{
  A (A, unsigned);
  A (const char *);
  ~A () { a1 (a4 ()); }
  void a1 (int);
  unsigned a2 ();
  char *a3 ();
  int a4 ();
};

template <typename T>
struct B
{
  A b;
  B (A x, int y = 1) : b (x.a3 (), x.a2 ()) { if (y & 1) b.a2 (); }
};

extern template struct B <char>;
A a1 ("foo"), a2 ("bar");
B<char> b1 (a1), b2 (a2, 8);

void
foo ()
{
  A a3 ("baz");
  B<char> b3 (a1), b4 (a3);
}
