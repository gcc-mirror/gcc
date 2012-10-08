// PR c++/52906
// { dg-do compile { target c++11 } }

[[gnu::deprecated]]; // { dg-error "does not declare anything" }
