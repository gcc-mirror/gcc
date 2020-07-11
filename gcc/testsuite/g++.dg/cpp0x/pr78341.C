// PR c++/78341
// { dg-do compile { target c++11 } }

alignas (alignas double // { dg-error "expected" }
// { dg-error "-:expected" "" { target *-*-* } .+1 }
