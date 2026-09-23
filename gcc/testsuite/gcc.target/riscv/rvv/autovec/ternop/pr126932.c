/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -ffp-contract=off" } */
/* { dg-require-effective-target rv64 } */

typedef struct { float x, y, z, w; } vec4;

vec4 mul_add(vec4 a, vec4 b, vec4 c)
{
  vec4 v;
  v.x = a.x + b.x * c.x;
  v.y = a.y + b.y * c.y;
  v.z = a.z + b.z * c.z;
  v.w = a.w + b.w * c.w;
  return v;
}

vec4 neg_mul_add(vec4 a, vec4 b, vec4 c)
{
  vec4 v;
  v.x = c.x - a.x * b.x;
  v.y = c.y - a.y * b.y;
  v.z = c.z - a.z * b.z;
  v.w = c.w - a.w * b.w;
  return v;
}

vec4 mul_sub(vec4 a, vec4 b, vec4 c)
{
  vec4 v;
  v.x = a.x * b.x - c.x;
  v.y = a.y * b.y - c.y;
  v.z = a.z * b.z - c.z;
  v.w = a.w * b.w - c.w;
  return v;
}

vec4 neg_mul_sub(vec4 a, vec4 b, vec4 c)
{
  vec4 v;
  v.x = -(a.x * b.x) - c.x;
  v.y = -(a.y * b.y) - c.y;
  v.z = -(a.z * b.z) - c.z;
  v.w = -(a.w * b.w) - c.w;
  return v;
}

/* No FMA variants should be emitted with -ffp-contract=off.  */
/* { dg-final { scan-assembler-not {\tvfmadd\.vv} } } */
/* { dg-final { scan-assembler-not {\tvfmsub\.vv} } } */
/* { dg-final { scan-assembler-not {\tvfnmadd\.vv} } } */
/* { dg-final { scan-assembler-not {\tvfnmsub\.vv} } } */
/* { dg-final { scan-assembler-not {\tvfmacc\.vv} } } */
/* { dg-final { scan-assembler-not {\tvfmsac\.vv} } } */
/* { dg-final { scan-assembler-not {\tvfnmacc\.vv} } } */
/* { dg-final { scan-assembler-not {\tvfnmsac\.vv} } } */
