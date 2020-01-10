// PR c++/93173
// { dg-do compile { target c++11 } }

template<typename> struct S1 {
  S1(S1 &);
  ~S1();
};
struct S2 {
  S1<void> x;
  int y;
  int z;
};
void f(S1<void> x, int y, int z) { new S2{x, y, z}; }
