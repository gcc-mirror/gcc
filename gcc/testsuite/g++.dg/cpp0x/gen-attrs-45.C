// PR c++/52906
// { dg-do compile { target c++11 } }

[[gnu::deprecated]]; // { dg-warning "attribute ignored" }
