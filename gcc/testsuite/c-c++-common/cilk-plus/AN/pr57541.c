/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int A[10];

int main () {
  char c = (char)N; /* { dg-error "undeclared" } */
  short s = (short)N;
  long l = (long)N;
  A[l:s:c];
}

/* { dg-message "note: each" "defined" { target *-*-* }  7 } */

