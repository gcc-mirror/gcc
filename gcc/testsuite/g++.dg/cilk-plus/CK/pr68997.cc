/* { dg-do compile } */
/* { dg-options "-std=c++11 -fcilkplus" } */

struct A1 {
  A1 () {}
  A1 (const A1&) {}
};

A1 fa1 () {
  return A1 ();
}

struct A2 {
  A2 () {}
  A2 (A2&&) {}
};

A2 fa2 () {
  A2 ();
}

struct B1 {
};

B1 fb1 () {
  return B1 ();
}

struct A3 {
  A3 (const B1&) {}
};

struct A4 {
  A4 (B1) {}
};

struct B2 {
  B2 () {}
  B2 (const B2&) {}
};

B2 fb2 () {
  return B2 ();
}

struct A5 {
  A5 (B2) {}
};

void t1 () {
  A1 a1 = _Cilk_spawn fa1 ();
}

void t2 () {
  A2 a2 = _Cilk_spawn fa2 ();
}

void t3 () {
  A3 a3 = _Cilk_spawn fb1 ();
}

void t4 () {
  A4 a4 = _Cilk_spawn fb1 ();
}

void t5 () {
  A5 a5 = _Cilk_spawn fb2 ();
}
