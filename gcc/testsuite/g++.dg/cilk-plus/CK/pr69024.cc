/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

struct A1 {
};

struct A2 {
  A2 () {}
  A2 (const A2&) {}
};

struct B1 {
  operator A1 () {
    return A1 ();
  }
};

B1 fb1 () {
  return B1 ();
}

struct B2 {
  operator A2 () {
    return A2 ();
  }
};

B2 fb2 () {
  return B2 ();
}

void t1 () {
  A1 a1 = _Cilk_spawn fb1 ();
}

void t2 () {
  A2 a2 = _Cilk_spawn fb2 ();
}
