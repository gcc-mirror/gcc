// PR target/116925
// { dg-do compile }
// { dg-options "-O2 -mavx -ffloat-store" }

typedef float V __attribute__((vector_size (16)));
V a, b, c;

void
foo ()
{
  c = a > b ? a : b;
}
