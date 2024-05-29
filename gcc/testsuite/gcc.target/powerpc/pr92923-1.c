/* { dg-do compile } */
/* { dg-options "-mvsx -O2 -fdump-tree-gimple" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Verify that overloaded built-ins for "and", "andc", "nor", "or" and "xor"
   do not produce VIEW_CONVERT_EXPR operations on their operands.  Like so:

  _1 = VIEW_CONVERT_EXPR<__vector signed int>(x);
  _2 = VIEW_CONVERT_EXPR<__vector signed int>(y);
  _3 = __builtin_altivec_vand (_1, _2);
  D.3245 = VIEW_CONVERT_EXPR<bcvec_t>(_3);
*/

typedef __attribute__((altivec(vector__))) __attribute__((altivec(bool__))) char bcvec_t;
typedef __attribute__((altivec(vector__))) signed char scvec_t;
typedef __attribute__((altivec(vector__))) unsigned char ucvec_t;

typedef __attribute__((altivec(vector__))) __attribute__((altivec(bool__))) short bsvec_t;
typedef __attribute__((altivec(vector__))) signed short ssvec_t;
typedef __attribute__((altivec(vector__))) unsigned short usvec_t;

typedef __attribute__((altivec(vector__))) __attribute__((altivec(bool__))) int bivec_t;
typedef __attribute__((altivec(vector__))) signed int sivec_t;
typedef __attribute__((altivec(vector__))) unsigned int uivec_t;

typedef __attribute__((altivec(vector__))) __attribute__((altivec(bool__))) long long bllvec_t;
typedef __attribute__((altivec(vector__))) signed long long sllvec_t;
typedef __attribute__((altivec(vector__))) unsigned long long ullvec_t;

typedef __attribute__((altivec(vector__))) double dvec_t;
typedef __attribute__((altivec(vector__))) float fvec_t;

bcvec_t
and_0 (bcvec_t x, bcvec_t y)
{
  return __builtin_vec_and (x, y);
}

scvec_t
and_1 (scvec_t x, scvec_t y)
{
  return __builtin_vec_and (x, y);
}

ucvec_t
and_2 (ucvec_t x, ucvec_t y)
{
  return __builtin_vec_and (x, y);
}

bsvec_t
and_3 (bsvec_t x, bsvec_t y)
{
  return __builtin_vec_and (x, y);
}

ssvec_t
and_4 (ssvec_t x, ssvec_t y)
{
  return __builtin_vec_and (x, y);
}

usvec_t
and_5 (usvec_t x, usvec_t y)
{
  return __builtin_vec_and (x, y);
}

bivec_t
and_6 (bivec_t x, bivec_t y)
{
  return __builtin_vec_and (x, y);
}

sivec_t
and_7 (sivec_t x, sivec_t y)
{
  return __builtin_vec_and (x, y);
}

uivec_t
and_8 (uivec_t x, uivec_t y)
{
  return __builtin_vec_and (x, y);
}

bllvec_t
and_9 (bllvec_t x, bllvec_t y)
{
  return __builtin_vec_and (x, y);
}

sllvec_t
and_10 (sllvec_t x, sllvec_t y)
{
  return __builtin_vec_and (x, y);
}

ullvec_t
and_11 (ullvec_t x, ullvec_t y)
{
  return __builtin_vec_and (x, y);
}

dvec_t
and_12 (dvec_t x, dvec_t y)
{
  return __builtin_vec_and (x, y);
}

fvec_t
and_13 (fvec_t x, fvec_t y)
{
  return __builtin_vec_and (x, y);
}

bcvec_t
andc_0 (bcvec_t x, bcvec_t y)
{
  return __builtin_vec_andc (x, y);
}

scvec_t
andc_1 (scvec_t x, scvec_t y)
{
  return __builtin_vec_andc (x, y);
}

ucvec_t
andc_2 (ucvec_t x, ucvec_t y)
{
  return __builtin_vec_andc (x, y);
}

bsvec_t
andc_3 (bsvec_t x, bsvec_t y)
{
  return __builtin_vec_andc (x, y);
}

ssvec_t
andc_4 (ssvec_t x, ssvec_t y)
{
  return __builtin_vec_andc (x, y);
}

usvec_t
andc_5 (usvec_t x, usvec_t y)
{
  return __builtin_vec_andc (x, y);
}

bivec_t
andc_6 (bivec_t x, bivec_t y)
{
  return __builtin_vec_andc (x, y);
}

sivec_t
andc_7 (sivec_t x, sivec_t y)
{
  return __builtin_vec_andc (x, y);
}

uivec_t
andc_8 (uivec_t x, uivec_t y)
{
  return __builtin_vec_andc (x, y);
}

bllvec_t
andc_9 (bllvec_t x, bllvec_t y)
{
  return __builtin_vec_andc (x, y);
}

sllvec_t
andc_10 (sllvec_t x, sllvec_t y)
{
  return __builtin_vec_andc (x, y);
}

ullvec_t
andc_11 (ullvec_t x, ullvec_t y)
{
  return __builtin_vec_andc (x, y);
}

dvec_t
andc_12 (dvec_t x, dvec_t y)
{
  return __builtin_vec_andc (x, y);
}

fvec_t
andc_13 (fvec_t x, fvec_t y)
{
  return __builtin_vec_andc (x, y);
}

bcvec_t
nor_0 (bcvec_t x, bcvec_t y)
{
  return __builtin_vec_nor (x, y);
}

scvec_t
nor_1 (scvec_t x, scvec_t y)
{
  return __builtin_vec_nor (x, y);
}

ucvec_t
nor_2 (ucvec_t x, ucvec_t y)
{
  return __builtin_vec_nor (x, y);
}

bsvec_t
nor_3 (bsvec_t x, bsvec_t y)
{
  return __builtin_vec_nor (x, y);
}

ssvec_t
nor_4 (ssvec_t x, ssvec_t y)
{
  return __builtin_vec_nor (x, y);
}

usvec_t
nor_5 (usvec_t x, usvec_t y)
{
  return __builtin_vec_nor (x, y);
}

bivec_t
nor_6 (bivec_t x, bivec_t y)
{
  return __builtin_vec_nor (x, y);
}

sivec_t
nor_7 (sivec_t x, sivec_t y)
{
  return __builtin_vec_nor (x, y);
}

uivec_t
nor_8 (uivec_t x, uivec_t y)
{
  return __builtin_vec_nor (x, y);
}

bllvec_t
nor_9 (bllvec_t x, bllvec_t y)
{
  return __builtin_vec_nor (x, y);
}

sllvec_t
nor_10 (sllvec_t x, sllvec_t y)
{
  return __builtin_vec_nor (x, y);
}

ullvec_t
nor_11 (ullvec_t x, ullvec_t y)
{
  return __builtin_vec_nor (x, y);
}

dvec_t
nor_12 (dvec_t x, dvec_t y)
{
  return __builtin_vec_nor (x, y);
}

fvec_t
nor_13 (fvec_t x, fvec_t y)
{
  return __builtin_vec_nor (x, y);
}

bcvec_t
or_0 (bcvec_t x, bcvec_t y)
{
  return __builtin_vec_or (x, y);
}

scvec_t
or_1 (scvec_t x, scvec_t y)
{
  return __builtin_vec_or (x, y);
}

ucvec_t
or_2 (ucvec_t x, ucvec_t y)
{
  return __builtin_vec_or (x, y);
}

bsvec_t
or_3 (bsvec_t x, bsvec_t y)
{
  return __builtin_vec_or (x, y);
}

ssvec_t
or_4 (ssvec_t x, ssvec_t y)
{
  return __builtin_vec_or (x, y);
}

usvec_t
or_5 (usvec_t x, usvec_t y)
{
  return __builtin_vec_or (x, y);
}

bivec_t
or_6 (bivec_t x, bivec_t y)
{
  return __builtin_vec_or (x, y);
}

sivec_t
or_7 (sivec_t x, sivec_t y)
{
  return __builtin_vec_or (x, y);
}

uivec_t
or_8 (uivec_t x, uivec_t y)
{
  return __builtin_vec_or (x, y);
}

bllvec_t
or_9 (bllvec_t x, bllvec_t y)
{
  return __builtin_vec_or (x, y);
}

sllvec_t
or_10 (sllvec_t x, sllvec_t y)
{
  return __builtin_vec_or (x, y);
}

ullvec_t
or_11 (ullvec_t x, ullvec_t y)
{
  return __builtin_vec_or (x, y);
}

dvec_t
or_12 (dvec_t x, dvec_t y)
{
  return __builtin_vec_or (x, y);
}

fvec_t
or_13 (fvec_t x, fvec_t y)
{
  return __builtin_vec_or (x, y);
}

bcvec_t
xor_0 (bcvec_t x, bcvec_t y)
{
  return __builtin_vec_xor (x, y);
}

scvec_t
xor_1 (scvec_t x, scvec_t y)
{
  return __builtin_vec_xor (x, y);
}

ucvec_t
xor_2 (ucvec_t x, ucvec_t y)
{
  return __builtin_vec_xor (x, y);
}

bsvec_t
xor_3 (bsvec_t x, bsvec_t y)
{
  return __builtin_vec_xor (x, y);
}

ssvec_t
xor_4 (ssvec_t x, ssvec_t y)
{
  return __builtin_vec_xor (x, y);
}

usvec_t
xor_5 (usvec_t x, usvec_t y)
{
  return __builtin_vec_xor (x, y);
}

bivec_t
xor_6 (bivec_t x, bivec_t y)
{
  return __builtin_vec_xor (x, y);
}

sivec_t
xor_7 (sivec_t x, sivec_t y)
{
  return __builtin_vec_xor (x, y);
}

uivec_t
xor_8 (uivec_t x, uivec_t y)
{
  return __builtin_vec_xor (x, y);
}

bllvec_t
xor_9 (bllvec_t x, bllvec_t y)
{
  return __builtin_vec_xor (x, y);
}

sllvec_t
xor_10 (sllvec_t x, sllvec_t y)
{
  return __builtin_vec_xor (x, y);
}

ullvec_t
xor_11 (ullvec_t x, ullvec_t y)
{
  return __builtin_vec_xor (x, y);
}

dvec_t
xor_12 (dvec_t x, dvec_t y)
{
  return __builtin_vec_xor (x, y);
}

fvec_t
xor_13 (fvec_t x, fvec_t y)
{
  return __builtin_vec_xor (x, y);
}

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "gimple" } } */
