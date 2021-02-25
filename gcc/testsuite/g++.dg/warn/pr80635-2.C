// PR tree-optimization/80635
// { dg-do compile { target c++17 } }
// { dg-options "-O2 -Wmaybe-uninitialized" }

#include <optional>

extern int get ();
extern void set (int);

struct A
{
  A () : m (get ()) {}
  ~A () { set (m); }	// { dg-bogus "may be used uninitialized in this function" }

  int m;
};

struct B
{
  B ();
  ~B ();
};

void func ()
{
  std::optional<A> maybe_a;
  std::optional<B> maybe_b;

  maybe_a.emplace ();
  maybe_b.emplace ();
}
