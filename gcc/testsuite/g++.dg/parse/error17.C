// PR c++/16965

template <typename T> struct B { 
  static int Bar(T); // { dg-error "" }
}; 
struct D : B<int>, B<char> {}; 
 
int i2 = D::Bar(2); // { dg-error "" }
