/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

typedef int v8si __attribute__ ((vector_size (32)));
typedef int v4si __attribute__ ((vector_size (16)));

void
lo (v8si *in, v4si *out)
{
  v8si a = *in + *in;
  *out = __builtin_shufflevector (a, a, 0, 1, 2, 3);
}

void
hi (v8si *in, v4si *out)
{
  v8si a = *in + *in;
  *out = __builtin_shufflevector (a, a, 4, 5, 6, 7);
}

/* { dg-final { scan-assembler-not {v[ls]e[0-9]+\.v\s+v[0-9]+,\s*-?[0-9]*\(sp\)} } } */
/* { dg-final { scan-assembler-not {v[ls][0-9]r\.v} } } */
/* { dg-final { scan-assembler-times {vslidedown\.vi} 1 } } */
