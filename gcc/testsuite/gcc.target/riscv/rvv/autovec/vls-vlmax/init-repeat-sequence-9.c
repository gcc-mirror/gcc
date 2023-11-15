/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvl1024b -mabi=lp64d -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef double vnx16df __attribute__ ((vector_size (16 * 8)));

/*
** f_vnx16df:
**   vsetivli\s+zero,\s*16,\s*e64,\s*m1,\s*ta,\s*ma
**   ...
**   vfmv\.v\.f\s+v[0-9]+,\s*[af]+[0-9]+
**   ...
**   vmv\.s\.x\s+v0,\s*[axt][0-9]+
**   vfmerge\.vfm\s+v[0-9]+,\s*v[0-9]+,\s*[af]+[0-9]+,\s*v0
**   vs1r\.v\s+v[0-9]+,\s*0\([axt][0-9]+\)
**   ret
*/
__attribute__ ((noipa)) void
f_vnx16df (double a, double b, double *out)
{
  vnx16df v = {
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
  };
  *(vnx16df *) out = v;
}
