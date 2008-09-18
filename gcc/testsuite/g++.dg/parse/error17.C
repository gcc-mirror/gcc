// { dg-options "-fshow-column" }
// PR c++/16965

template <typename T> struct B { 
  static int Bar(T); // { dg-error "14:candidates are: " "1" }
  // { dg-error "14:with T = int" "2" { target *-*-* } 5 }
}; 
struct D : B<int>, B<char> {}; 
 
int i2 = D::Bar(2); // { dg-error "13:reference to 'Bar' is ambiguous" }
// { dg-error "10:reference to 'Bar' is ambiguous" "2" { target *-*-* } 10 }
