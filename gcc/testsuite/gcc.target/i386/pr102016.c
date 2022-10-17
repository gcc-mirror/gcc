/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
typedef char v8qi __attribute__((vector_size (8)));
typedef char v16qi __attribute__((vector_size (16)));
v8qi
foo_wb_128 (v16qi x)
{
  return __builtin_shufflevector (x, x,
				  0, 2, 4, 6, 8, 10, 12, 14);
}
