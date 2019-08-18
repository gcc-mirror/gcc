/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32d" } */
struct s1 { int : 0; float f; int i; int : 0; };

void dummy(float, int);

void f(struct s1 s) { /* { dg-warning "flattened struct" } */
  dummy(s.f + 1.0, s.i + 1);
}
