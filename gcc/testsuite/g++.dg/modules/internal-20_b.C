// PR c++/122994
// { dg-additional-options "-fmodules" }

static int internal() { return 42; }
int merge = []{ return internal(); }();

import m;

int& use_a = a;
int& use_b = b;
int& use_c = c<int>;  // { dg-message "required from here" }
int& use_d = d<int>;  // { dg-bogus "" }
int& use_d2 = d<double>;  // { dg-message "required from here" }
int& use_e = e;
int& use_f = f;
int& use_merge = merge;

// { dg-error "instantiation exposes TU-local entity" "" { target *-*-* } 0 }
