// PR c++/84972
// { dg-additional-options "-w" }

char(a[])({.a = 0});  // { dg-error "designated initializer" }
