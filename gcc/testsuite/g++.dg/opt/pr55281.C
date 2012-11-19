// PR tree-optimization/55281
// { dg-do compile }
// { dg-options "-Ofast" }

typedef float VF __attribute__((vector_size (16)));

VF x;

void
foo (void)
{
  VF a, b, c;
  a = (VF) { 1.0, 2.0, 3.0, 4.0 };
  b = (VF) { 5.0, 6.0, 7.0, 8.0 };
  c = (VF) { 0.0, 0.0, 0.0, 0.0 };
  x = c == ((VF) { 0.0, 0.0, 0.0, 0.0 }) ? a : b;
}
