// PR c++/93789 - ICE with invalid array bounds.
// { dg-do compile }
// { dg-options "" }

void
f ()
{
  const int tbl[(long) "h"] = { 12 }; // { dg-error "size of array .tbl. is not an integral constant-expression" }
}				      // { dg-warning "narrowing conversion" "" { target c++11 } .-1 }
