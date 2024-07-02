// C++26 P3144R2 - Deleting a Pointer to an Incomplete Type Should be Ill-formed
// { dg-do compile { target c++26 } }
// { dg-options "-Wno-delete-incomplete" }

struct S;
struct T;
struct U;

void
foo (S *p, T *q, U *r, S *s, T *t, U *u)
{
  delete p;	// { dg-bogus "operator 'delete' used on incomplete type" }
  delete q;	// { dg-bogus "operator 'delete' used on incomplete type" }
  delete r;	// { dg-bogus "operator 'delete' used on incomplete type" }
  delete[] s;	// { dg-bogus "operator 'delete \\\[\\\]' used on incomplete type" }
  delete[] t;	// { dg-bogus "operator 'delete \\\[\\\]' used on incomplete type" }
  delete[] u;	// { dg-bogus "operator 'delete \\\[\\\]' used on incomplete type" }
}

struct S
{
  int s;
};

struct T
{
  int t;
  ~T () {}
};

struct U
{
  int u;
  void operator delete (void *) noexcept;
  void operator delete[] (void *) noexcept;
};
