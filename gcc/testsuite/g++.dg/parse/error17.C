// { dg-options "-fshow-column" }
// PR c++/16965

template <typename T> struct B { 
  static int Bar(T); // { dg-error "14:candidates are: |with T = int" }
}; 
struct D : B<int>, B<char> {}; 
 
int i2 = D::Bar(2); // { dg-error "10:reference to 'Bar' is ambiguous" }
