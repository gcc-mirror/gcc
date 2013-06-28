/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int A[10];

int main () {

  /* C compiler uses the term "undeclared" whereas C++ compiler uses
    "not declared".  Thus, grepping for declared seem to be the easiest.  */
  char c = (char)N; /* { dg-error "declared" } */
  short s = (short)N;
  long l = (long)N;
  A[l:s:c];
}

/* { dg-message "note: each" "defined" { target c }  10 } */

