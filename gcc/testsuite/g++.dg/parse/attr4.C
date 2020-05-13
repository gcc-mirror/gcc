// PR c++/93684 - ICE-on-invalid with broken attribute.

[[a:: // { dg-error "expected" }
  // { dg-error "-:expected" "" { target c++11 } .+1 }
