/* PR c/44517: Improve diagnostic for misspelled typename in function declaration. */
int f1(int x, pid_t y, long z, in t) {
/* { dg-error "unknown type name 'pid_t'" "pid_t" { target *-*-* } .-1 } */
/* { dg-error "unknown type name 'in'" "in" { target *-*-* } .-2 } */
  return x + y + z + t;
}

int f2(int x, lon y, long z, ...){ /* { dg-error "unknown type name 'lon'" } */
  return;
}

void f3(int n, int a[n], pid_t x); /* { dg-error "unknown type name 'pid_t'" } */
void f4() {}
void f5(int a, *b); /* { dg-error "expected declaration specifiers or" } */
void f6(int a, b);  /* { dg-error "unknown type name 'b'" } */
void f7(int a, goto b); /* { dg-error "expected declaration specifiers or" } */
void f8(int a, in goto); /* { dg-error "unknown type name 'in'" } */
void f9(int a, in 1); /* { dg-error "unknown type name 'in'" } */
