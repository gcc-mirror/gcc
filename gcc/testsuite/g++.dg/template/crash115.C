// PR c++/56534

template < struct template rebind < > // { dg-error "expected|must follow" }
// { dg-error "-:expected" "" { target *-*-* } .+1 }
