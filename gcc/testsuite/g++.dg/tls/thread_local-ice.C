// PR c++/55801
// { dg-options "-std=c++11" }
// { dg-require-effective-target tls }

class C;
thread_local C O, O2 = O;   // { dg-error "incomplete" }
