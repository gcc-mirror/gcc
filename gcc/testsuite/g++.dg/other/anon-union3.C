// PR c++/32054

class C
{
  auto union      // { dg-error "storage class" "" { target { ! c++11 } } }
    {		  // { dg-error "auto" "" { target c++11 } .-1 }
      int a;
    };            // { dg-error "multiple types" "" { target c++11 } }
  register union  // { dg-error "storage class" }
    {
      int b;
    };
  static union    // { dg-error "storage class" }
    {
      int c;
    };
  extern union    // { dg-error "storage class" }
    {
      int d;
    };
  mutable union   // { dg-error "storage class" }
    {
      int e;
    };
};
