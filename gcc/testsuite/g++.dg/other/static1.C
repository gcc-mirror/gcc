// PR c++/9574
// Origin: fche@redhat.com and bangerth@dealii.org
// The new parser ICE on this test and then it could 
//  not find z in bar::bar().

// { dg-do compile }

struct X {
  void operator[](const int& __k);
};
struct foo {
  static X x;
};
struct bar {
  int z;
  bar () { foo::x[z]; }
};
