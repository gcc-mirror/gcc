/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2 -fdump-tree-gimple" } */

/* Verify that overloaded built-ins for "eqv", "nand" and "orc" do not
   produce VIEW_CONVERT_EXPR operations on their operands.  Like so:

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
eqv_0 (bcvec_t x, bcvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

scvec_t
eqv_1 (scvec_t x, scvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

ucvec_t
eqv_2 (ucvec_t x, ucvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

bsvec_t
eqv_3 (bsvec_t x, bsvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

ssvec_t
eqv_4 (ssvec_t x, ssvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

usvec_t
eqv_5 (usvec_t x, usvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

bivec_t
eqv_6 (bivec_t x, bivec_t y)
{
  return __builtin_vec_eqv (x, y);
}

sivec_t
eqv_7 (sivec_t x, sivec_t y)
{
  return __builtin_vec_eqv (x, y);
}

uivec_t
eqv_8 (uivec_t x, uivec_t y)
{
  return __builtin_vec_eqv (x, y);
}

bllvec_t
eqv_9 (bllvec_t x, bllvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

sllvec_t
eqv_10 (sllvec_t x, sllvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

ullvec_t
eqv_11 (ullvec_t x, ullvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

dvec_t
eqv_12 (dvec_t x, dvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

fvec_t
eqv_13 (fvec_t x, fvec_t y)
{
  return __builtin_vec_eqv (x, y);
}

bcvec_t
nand_0 (bcvec_t x, bcvec_t y)
{
  return __builtin_vec_nand (x, y);
}

scvec_t
nand_1 (scvec_t x, scvec_t y)
{
  return __builtin_vec_nand (x, y);
}

ucvec_t
nand_2 (ucvec_t x, ucvec_t y)
{
  return __builtin_vec_nand (x, y);
}

bsvec_t
nand_3 (bsvec_t x, bsvec_t y)
{
  return __builtin_vec_nand (x, y);
}

ssvec_t
nand_4 (ssvec_t x, ssvec_t y)
{
  return __builtin_vec_nand (x, y);
}

usvec_t
nand_5 (usvec_t x, usvec_t y)
{
  return __builtin_vec_nand (x, y);
}

bivec_t
nand_6 (bivec_t x, bivec_t y)
{
  return __builtin_vec_nand (x, y);
}

sivec_t
nand_7 (sivec_t x, sivec_t y)
{
  return __builtin_vec_nand (x, y);
}

uivec_t
nand_8 (uivec_t x, uivec_t y)
{
  return __builtin_vec_nand (x, y);
}

bllvec_t
nand_9 (bllvec_t x, bllvec_t y)
{
  return __builtin_vec_nand (x, y);
}

sllvec_t
nand_10 (sllvec_t x, sllvec_t y)
{
  return __builtin_vec_nand (x, y);
}

ullvec_t
nand_11 (ullvec_t x, ullvec_t y)
{
  return __builtin_vec_nand (x, y);
}

dvec_t
nand_12 (dvec_t x, dvec_t y)
{
  return __builtin_vec_nand (x, y);
}

fvec_t
nand_13 (fvec_t x, fvec_t y)
{
  return __builtin_vec_nand (x, y);
}

bcvec_t
orc_0 (bcvec_t x, bcvec_t y)
{
  return __builtin_vec_orc (x, y);
}

scvec_t
orc_1 (scvec_t x, scvec_t y)
{
  return __builtin_vec_orc (x, y);
}

ucvec_t
orc_2 (ucvec_t x, ucvec_t y)
{
  return __builtin_vec_orc (x, y);
}

bsvec_t
orc_3 (bsvec_t x, bsvec_t y)
{
  return __builtin_vec_orc (x, y);
}

ssvec_t
orc_4 (ssvec_t x, ssvec_t y)
{
  return __builtin_vec_orc (x, y);
}

usvec_t
orc_5 (usvec_t x, usvec_t y)
{
  return __builtin_vec_orc (x, y);
}

bivec_t
orc_6 (bivec_t x, bivec_t y)
{
  return __builtin_vec_orc (x, y);
}

sivec_t
orc_7 (sivec_t x, sivec_t y)
{
  return __builtin_vec_orc (x, y);
}

uivec_t
orc_8 (uivec_t x, uivec_t y)
{
  return __builtin_vec_orc (x, y);
}

bllvec_t
orc_9 (bllvec_t x, bllvec_t y)
{
  return __builtin_vec_orc (x, y);
}

sllvec_t
orc_10 (sllvec_t x, sllvec_t y)
{
  return __builtin_vec_orc (x, y);
}

ullvec_t
orc_11 (ullvec_t x, ullvec_t y)
{
  return __builtin_vec_orc (x, y);
}

dvec_t
orc_12 (dvec_t x, dvec_t y)
{
  return __builtin_vec_orc (x, y);
}

fvec_t
orc_13 (fvec_t x, fvec_t y)
{
  return __builtin_vec_orc (x, y);
}

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "gimple" } } */
