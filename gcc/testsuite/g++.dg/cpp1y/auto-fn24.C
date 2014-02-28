// PR c++/60314
// { dg-options "-std=c++1y -g" }

// fine
decltype(auto) qux() { return 42; }

struct foo
{
  // also ICEs if not static 
  static decltype(auto) bar()
  { return 42; }
};
