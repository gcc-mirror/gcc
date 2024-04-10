// PR c++/112437
// { dg-do compile }

struct S {};

S
foo (S s)
try {
  throw s;
}
catch (...) {
  throw s;
}
