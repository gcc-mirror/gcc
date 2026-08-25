/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-forwprop1" } */
/* { dg-additional-options "-march=armv8.2-a+sve -msve-vector-bits=scalable" } */

typedef float v4sf __attribute__ ((vector_size (16)));

float __GIMPLE ()
reduc_fmax (v4sf x)
{
  v4sf c;
  v4sf v;
  float res;

  c = _Literal (v4sf) { 1.0f, 1.0f, 1.0f, 1.0f };
  v = .FMAX (x, c);
  res = .REDUC_FMAX (v);
  return res;
}

float __GIMPLE ()
reduc_fmin (v4sf x)
{
  v4sf c;
  v4sf v;
  float res;

  c = _Literal (v4sf) { 1.0f, 1.0f, 1.0f, 1.0f };
  v = .FMIN (x, c);
  res = .REDUC_FMIN (v);
  return res;
}

float __GIMPLE ()
reduc_fmax_scalable (__SVFloat32_t x)
{
  __SVFloat32_t c;
  __SVFloat32_t v;
  float res;

  c = _Literal (__SVFloat32_t) 0;
  v = .FMAX (x, c);
  res = .REDUC_FMAX (v);
  return res;
}

float __GIMPLE ()
reduc_fmin_scalable (__SVFloat32_t x)
{
  __SVFloat32_t c;
  __SVFloat32_t v;
  float res;

  c = _Literal (__SVFloat32_t) 0;
  v = .FMIN (x, c);
  res = .REDUC_FMIN (v);
  return res;
}

/* Do not introduce a second reduction when the constant reduction cannot
   fold.  */
/* { dg-final { scan-tree-dump-times {\.REDUC_FMAX} 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_FMIN} 2 "forwprop1" } } */
