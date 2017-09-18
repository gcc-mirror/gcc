/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef short int __attribute__ ((vector_size (16))) v8hi;

v8hi
mla8hi (v8hi v0, v8hi v1, short int v2)
{
  /* { dg-final { scan-assembler "mla\\tv\[0-9\]\+\\.8h, v\[0-9\]\+\\.8h, v\[0-9\]\+\\.h\\\[0\\\]" } } */
  return v0 + v1 * v2;
}


v8hi
mls8hi (v8hi v0, v8hi v1, short int v2)
{
  /* { dg-final { scan-assembler "mls\\tv\[0-9\]\+\\.8h, v\[0-9\]\+\\.8h, v\[0-9\]\+\\.h\\\[0\\\]" } } */
  return v0 - v1 * v2;
}

typedef short int __attribute__ ((vector_size (8))) v4hi;

v4hi
mla4hi (v4hi v0, v4hi v1, short int v2)
{
  /* { dg-final { scan-assembler "mla\\tv\[0-9\]\+\\.4h, v\[0-9\]\+\\.4h, v\[0-9\]\+\\.h\\\[0\\\]" } } */
  return v0 + v1 * v2;
}

v4hi
mls4hi (v4hi v0, v4hi v1, short int v2)
{
  /* { dg-final { scan-assembler "mls\\tv\[0-9\]\+\\.4h, v\[0-9\]\+\\.4h, v\[0-9\]\+\\.h\\\[0\\\]" } } */
  return v0 - v1 * v2;
}

typedef int __attribute__ ((vector_size (16))) v4si;

v4si
mla4si (v4si v0, v4si v1, int v2)
{
  /* { dg-final { scan-assembler "mla\\tv\[0-9\]\+\\.4s, v\[0-9\]\+\\.4s, v\[0-9\]\+\\.s\\\[0\\\]" } } */
  return v0 + v1 * v2;
}

v4si
mls4si (v4si v0, v4si v1, int v2)
{
  /* { dg-final { scan-assembler "mls\\tv\[0-9\]\+\\.4s, v\[0-9\]\+\\.4s, v\[0-9\]\+\\.s\\\[0\\\]" } } */
  return v0 - v1 * v2;
}

typedef int __attribute__((vector_size (8))) v2si;

v2si
mla2si (v2si v0, v2si v1, int v2)
{
  /* { dg-final { scan-assembler "mla\\tv\[0-9\]\+\\.2s, v\[0-9\]\+\\.2s, v\[0-9\]\+\\.s\\\[0\\\]" } } */
  return v0 + v1 * v2;
}

v2si
mls2si (v2si v0, v2si v1, int v2)
{
  /* { dg-final { scan-assembler "mls\\tv\[0-9\]\+\\.2s, v\[0-9\]\+\\.2s, v\[0-9\]\+\\.s\\\[0\\\]" } } */
  return v0 - v1 * v2;
}
