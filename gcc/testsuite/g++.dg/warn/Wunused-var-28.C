// PR c++/82799
// { dg-do compile }
// { dg-options "-Wunused-but-set-variable" }

enum E { b };     
struct C {
  template <E>
  int foo ()
  {
    const bool i = 0;		// { dg-bogus "set but not used" }
    const int r = i ? 7 : 9;
    return r;
  }
  void bar () { foo <b> (); }
};
