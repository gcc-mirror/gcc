/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo() {
  int x;
  float& q = reinterpret_cast<float&> (x);  /* { dg-message "initialized" } */
  q = 1.0; /* { dg-warning "does break strict-aliasing" } */
  return x;
}

/* { dg-message "dereferencing type-punned" "" { target *-*-* } 7 } */
