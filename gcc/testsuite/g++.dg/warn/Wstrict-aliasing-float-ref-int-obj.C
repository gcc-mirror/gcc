/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo() {
  int x;
  float& q = reinterpret_cast<float&> (x);  /* { dg-message "dereferencing type-punned" "" { target *-*-* } } */
  q = 1.0; /* { dg-warning "does break strict-aliasing" "" { xfail *-*-* } } */
  return x;
}

/* { dg-message "initialized" "" { xfail *-*-* } 7 } */
