/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo() {
  int x;
  float& q = reinterpret_cast<float&> (x);  /* { dg-message "dereferencing type-punned" "deref" { target *-*-* } } */
  q = 1.0; /* { dg-warning "does break strict-aliasing" "strict-aliasing" { xfail *-*-* } } */
  return x;
}

/* { dg-message "initialized" "note" { xfail *-*-* } 7 } */
