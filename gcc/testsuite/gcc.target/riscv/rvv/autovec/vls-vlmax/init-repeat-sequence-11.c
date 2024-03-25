/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvl1024b -mabi=lp64d -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef double vnx32df __attribute__ ((vector_size (32 * 8)));

/*
** f_vnx32df:
**   vsetvli\s+[axt][0-9]+\s*,zero,\s*e64,\s*m2,\s*ta,\s*ma
**   ...
**   vfmv\.v\.f\s+v[0-9]+,\s*[af]+[0-9]+
**   ...
**   vmv\.s\.x\s+v0,\s*[axt][0-9]+
**   vfmerge\.vfm\s+v[0-9]+,\s*v[0-9]+,\s*[af]+[0-9]+,\s*v0
**   vs2r\.v\s+v[0-9]+,\s*0\([axt][0-9]+\)
**   ret
*/
__attribute__ ((noipa)) void
f_vnx32df (double a, double b, double *out)
{
  vnx32df v = {
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
  };
  *(vnx32df *) out = v;
}
