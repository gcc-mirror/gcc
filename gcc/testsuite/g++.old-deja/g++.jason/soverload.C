// { dg-do assemble  }
// Bug: dfs_pushdecls doesn't create an overload list for member functions,
//   like it claims to.

struct A
{
  static int foo (int);
  static int foo (int, int);
  void bar () { foo (1, 2); }	// { dg-bogus "" } broken overloading
};
