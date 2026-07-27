// PR c++/125343
// { dg-do compile { target c++20 } }

void f1()
{
  []<struct S {}>() {}; // { dg-error "types may not be defined in parameter types" }
  // { dg-error "definition of .struct f1..::<lambda>::S. inside template parameter list" "" { target *-*-* } .-1 }
  [] {};
}

void f2()
{
  []<struct {}>() {}; // { dg-error "types may not be defined in parameter types" }
  // { dg-error "definition of .struct f2..::<lambda>::<unnamed>. inside template parameter list" "" { target *-*-* } .-1 }
}

void f3()
{
  []<class T = struct S {}>() {};
  // { dg-error "definition of .struct f3..::<lambda>::S. inside template parameter list" "" { target *-*-* } .-1 }
}

void f4()
{
  []<template <union U {}> class T>() {}; // { dg-error "types may not be defined in parameter types" }
  // { dg-error "definition of .union f4..::<lambda>::U. inside template parameter list" "" { target *-*-* } .-1 }
}

void f5()
{
  []<struct S {}; // { dg-error "types may not be defined in parameter types" }
  // { dg-error "definition of .struct f5..::<lambda>::S. inside template parameter list" "" { target *-*-* } .-1 }
  // { dg-error "expected .>. before .;. token" "" { target *-*-* } .-2 }
  // { dg-error "expected '\\\{' before ';' token" "" { target *-*-* } .-3 }
}
