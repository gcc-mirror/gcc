// { dg-options "-fshow-column" }
// PR c++/16965

template <typename T> struct B { 
  static int Bar(T); // { dg-error "19: error: candidates are: |19: error:  " }
}; 
struct D : B<int>, B<char> {}; 
 
int i2 = D::Bar(2); // { dg-error "13: error: reference to 'Bar' is ambiguous|10: error: reference to 'Bar' is ambiguous" }
