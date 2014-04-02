// PR c++/60314
// { dg-do compile { target c++1y } }
// { dg-options "-g" }

// fine
decltype(auto) qux() { return 42; }

struct foo
{
  // also ICEs if not static 
  static decltype(auto) bar()
  { return 42; }
};
