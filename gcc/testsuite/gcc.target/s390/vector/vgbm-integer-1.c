/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE MASK nor
   via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE BYTE MASK.  */

typedef char v8qi __attribute__ ((vector_size (8)));
typedef char v16qi __attribute__ ((vector_size (16)));

typedef short v4hi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));

typedef int v2si __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));

typedef long long v1di __attribute__ ((vector_size (8)));
typedef long long v2di __attribute__ ((vector_size (16)));

/*
** test_v8qi:
**     vgbm	%v24,20480
**     br	%r14
*/

v8qi
test_v8qi (void)
{
  return (v8qi){0, -1, 0, -1, 0, 0, 0, 0};
}

/*
** test_v16qi:
**     vgbm	%v24,20560
**     br	%r14
*/

v16qi
test_v16qi (void)
{
  return (v16qi){0, -1, 0, -1, 0, 0, 0, 0, 0, -1, 0, -1, 0, 0, 0, 0};
}

/*
** test_v4hi:
**     vgbm	%v24,20480
**     br	%r14
*/

v4hi
test_v4hi (void)
{
  return (v4hi){0xff, 0xff, 0, 0};
}

/*
** test_v8hi:
**     vgbm	%v24,20560
**     br	%r14
*/

v8hi
test_v8hi (void)
{
  return (v8hi){0xff, 0xff, 0, 0, 0xff, 0xff, 0, 0};
}

/*
** test_v2si:
**     vgbm	%v24,20480
**     br	%r14
*/

v2si
test_v2si (void)
{
  return (v2si){0xff00ff, 0};
}

/*
** test_v4si:
**     vgbm	%v24,20560
**     br	%r14
*/

v4si
test_v4si (void)
{
  return (v4si){0xff00ff, 0, 0xff00ff, 0};
}

/*
** test_v1di:
**     vgbm	%v24,20480
**     br	%r14
*/

v1di
test_v1di (void)
{
  return (v1di){0xff00ff00000000};
}

/*
** test_v2di:
**     vgbm	%v24,20560
**     br	%r14
*/

v2di
test_v2di (void)
{
  return (v2di){0xff00ff00000000, 0xff00ff00000000};
}
