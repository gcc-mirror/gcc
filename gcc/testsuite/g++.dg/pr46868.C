// PR c++/46868
// { dg-do compile }

template < int > struct S { S < // { dg-error "" }
  // { dg-error "-:expected" "" { target *-*-* } .+1 }
