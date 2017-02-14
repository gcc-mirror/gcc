// ++i is always an lvalue
void
f()
{
  bool i = 0;
  ++i = 3;
  // { dg-warning "deprecated" "" { target { ! c++1z } } 6 }
  // { dg-error "forbidden" "" { target c++1z } 6 }
}
