// PR c++/55801
// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }

class C;
thread_local C O, O2 = O;   // { dg-error "incomplete" }
