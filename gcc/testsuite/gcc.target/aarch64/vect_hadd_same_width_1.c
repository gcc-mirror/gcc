/* Same-width averages of signed types whose overflow is undefined.  */
/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef short v8hi __attribute__ ((vector_size (16)));
typedef signed char v16qi __attribute__ ((vector_size (16)));
typedef unsigned short v8hu __attribute__ ((vector_size (16)));

void
ceil4 (int *restrict r, int *restrict a, int *restrict b, int *restrict c,
       int *restrict d, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = (a[i] + b[i] + c[i] + d[i] + 1) >> 1;
}

void
floor2 (int *restrict r, int *restrict a, int *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = (a[i] + b[i]) >> 1;
}

v8hi ceil_v8hi (v8hi a, v8hi b) { return (a + b + 1) >> 1; }
v8hi floor_v8hi (v8hi a, v8hi b) { return (a + b) >> 1; }
v16qi ceil_v16qi (v16qi a, v16qi b) { return (a + b + 1) >> 1; }
v16qi floor_v16qi (v16qi a, v16qi b) { return (a + b) >> 1; }

/* Unsigned sums can wrap.  */

void
ceil_unsigned (unsigned *restrict r, unsigned *restrict a,
	       unsigned *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = (a[i] + b[i] + 1) >> 1;
}

v8hu floor_v8hu (v8hu a, v8hu b) { return (a + b) >> 1; }

/* { dg-final { scan-assembler {\tsrhadd\tv[0-9]+\.4s,} } } */
/* { dg-final { scan-assembler {\tshadd\tv[0-9]+\.4s,} } } */
/* { dg-final { scan-assembler-times {\tsrhadd\tv[0-9]+\.8h,} 1 } } */
/* { dg-final { scan-assembler-times {\tshadd\tv[0-9]+\.8h,} 1 } } */
/* { dg-final { scan-assembler-times {\tsrhadd\tv[0-9]+\.16b,} 1 } } */
/* { dg-final { scan-assembler-times {\tshadd\tv[0-9]+\.16b,} 1 } } */
/* { dg-final { scan-assembler-not {\tu(r)?hadd\t} } } */
