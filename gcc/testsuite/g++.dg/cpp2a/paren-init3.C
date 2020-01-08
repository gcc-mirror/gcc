// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++2a } }

class a {
  int b{};
};
class c {
  c();
  a d;
};
c::c() {}
