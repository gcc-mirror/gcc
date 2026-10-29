/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Generate and check most of the vector logical instruction combinations that
   may or may not generate xxeval to do a fused operation on power10.  */

#include <stddef.h>
#include <stdlib.h>
#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>

static int errors = 0;
static int tests  = 0;
#endif

typedef vector unsigned int	vector_t;
typedef unsigned int		scalar_t;

/* Vector logical functions.  */
static inline vector_t
vector_and (vector_t x, vector_t y)
{
  return x & y;
}

static inline vector_t
vector_or (vector_t x, vector_t y)
{
  return x | y;
}

static inline vector_t
vector_xor (vector_t x, vector_t y)
{
  return x ^ y;
}

static inline vector_t
vector_andc (vector_t x, vector_t y)
{
  return x & ~y;
}

static inline vector_t
vector_orc (vector_t x, vector_t y)
{
  return x | ~y;
}

static inline vector_t
vector_nand (vector_t x, vector_t y)
{
  return ~(x & y);
}

static inline vector_t
vector_nor (vector_t x, vector_t y)
{
  return ~(x | y);
}

static inline vector_t
vector_eqv (vector_t x, vector_t y)
{
  return ~(x ^ y);
}

/* Scalar logical functions.  */
static inline scalar_t
scalar_and (scalar_t x, scalar_t y)
{
  return x & y;
}

static inline scalar_t
scalar_or (scalar_t x, scalar_t y)
{
  return x | y;
}

static inline scalar_t
scalar_xor (scalar_t x, scalar_t y)
{
  return x ^ y;
}

static inline scalar_t
scalar_andc (scalar_t x, scalar_t y)
{
  return x & ~y;
}

static inline scalar_t
scalar_orc (scalar_t x, scalar_t y)
{
  return x | ~y;
}

static inline scalar_t
scalar_nand (scalar_t x, scalar_t y)
{
  return ~(x & y);
}

static inline scalar_t
scalar_nor (scalar_t x, scalar_t y)
{
  return ~(x | y);
}

static inline scalar_t
scalar_eqv (scalar_t x, scalar_t y)
{
  return ~(x ^ y);
}


/*
 * Generate one function for each combination that we are checking.  Do 4
 * operations:
 *
 * Use FPR regs that should generate either XXEVAL or XXL* insns;
 * Use Altivec registers than may generated fused V* insns;
 * Use VSX registers, insure fusing it not done via asm; (and)
 * Use GPR registers on scalar operations.
 */

#ifdef DEBUG
#define TRACE(INNER, OUTER)						\
  do {									\
    tests++;								\
    printf ("%s_%s\n", INNER, OUTER);					\
    fflush (stdout);							\
  } while (0)								\

#define FAILED(INNER, OUTER)						\
  do {									\
    errors++;								\
    printf ("%s_%s failed\n", INNER, OUTER);				\
    fflush (stdout);							\
  } while (0)								\

#else
#define TRACE(INNER, OUTER)
#define FAILED(INNER, OUTER)	abort ()
#endif

#define FUSED_FUNC(INNER, OUTER)					\
static void								\
INNER ## _ ## OUTER (vector_t a, vector_t b, vector_t c)		\
{									\
  vector_t f_a, f_b, f_c, f_r, f_t;					\
  vector_t v_a, v_b, v_c, v_r, v_t;					\
  vector_t w_a, w_b, w_c, w_r, w_t;					\
  scalar_t s_a, s_b, s_c, s_r, s_t;					\
									\
  TRACE (#INNER, #OUTER);						\
									\
  f_a = a;								\
  f_b = b;								\
  f_c = c;								\
									\
  __asm__ (" # fpr regs: %x0,%x1,%x2 " #INNER "_" #OUTER		\
	   : "+d" (f_a),						\
	     "+d" (f_b),						\
	     "+d" (f_c));						\
									\
  f_t = vector_ ## INNER (f_b, f_c);					\
  f_r = vector_ ## OUTER (f_a, f_t);					\
									\
  __asm__ (" # fpr regs result: %x0 " #INNER "_" #OUTER			\
	   : "+d" (f_r));						\
									\
  v_a = a;								\
  v_b = b;								\
  v_c = c;								\
									\
  __asm__ (" # altivec regs: %x0,%x1,%x2 " #INNER "_" #OUTER		\
	   : "+v" (v_a),						\
	     "+v" (v_b),						\
	     "+v" (v_c));						\
									\
  v_t = vector_ ## INNER (v_b, v_c);					\
  v_r = vector_ ## OUTER (v_a, v_t);					\
									\
  __asm__ (" # altivec regs result: %x0 " #INNER "_" #OUTER		\
	   : "+v" (v_r));						\
									\
  w_a = a;								\
  w_b = b;								\
  w_c = c;								\
									\
  __asm__ (" # vsx regs: %x0,%x1,%x2 " #INNER "_" #OUTER		\
	   : "+wa" (w_a),						\
	     "+wa" (w_b),						\
	     "+wa" (w_c));						\
									\
  w_t = vector_ ## INNER (w_b, w_c);					\
  __asm__ ("nop # break vsx fusion reg %x0" : "+wa" (w_t));		\
  w_r = vector_ ## OUTER (w_a, w_t);					\
									\
  __asm__ (" # vsx regs result: %x0 " #INNER "_" #OUTER			\
	   : "+wa" (w_r));						\
									\
  s_a = a[0];								\
  s_b = b[0];								\
  s_c = c[0];								\
									\
  __asm__ (" # gpr regs: %0,%1,%2 " #INNER "_" #OUTER			\
	   : "+r" (s_a),						\
	     "+r" (s_b),						\
	     "+r" (s_c));						\
									\
  s_t = scalar_ ## INNER (s_b, s_c);					\
  s_r = scalar_ ## OUTER (s_a, s_t);					\
									\
  __asm__ (" # gpr regs result: %0 " #INNER "_" #OUTER			\
	   : "+r" (s_r));						\
									\
  if (!vec_all_eq (w_r, f_r)						\
      || !vec_all_eq (w_r, v_r)						\
      || s_r != w_r[0])							\
    FAILED (#INNER, #OUTER);						\
									\
  return;								\
}

FUSED_FUNC (and,  and)
FUSED_FUNC (andc, and)
FUSED_FUNC (eqv,  and)
FUSED_FUNC (nand, and)
FUSED_FUNC (nor,  and)
FUSED_FUNC (or,   and)
FUSED_FUNC (orc,  and)
FUSED_FUNC (xor,  and)

FUSED_FUNC (and,  andc)
FUSED_FUNC (andc, andc)
FUSED_FUNC (eqv,  andc)
FUSED_FUNC (nand, andc)
FUSED_FUNC (nor,  andc)
FUSED_FUNC (or,   andc)
FUSED_FUNC (orc,  andc)
FUSED_FUNC (xor,  andc)

FUSED_FUNC (and,  eqv)
FUSED_FUNC (andc, eqv)
FUSED_FUNC (eqv,  eqv)
FUSED_FUNC (nand, eqv)
FUSED_FUNC (nor,  eqv)
FUSED_FUNC (or,   eqv)
FUSED_FUNC (orc,  eqv)
FUSED_FUNC (xor,  eqv)

FUSED_FUNC (and,  nand)
FUSED_FUNC (andc, nand)
FUSED_FUNC (eqv,  nand)
FUSED_FUNC (nand, nand)
FUSED_FUNC (nor,  nand)
FUSED_FUNC (or,   nand)
FUSED_FUNC (orc,  nand)
FUSED_FUNC (xor,  nand)

FUSED_FUNC (and,  nor)
FUSED_FUNC (andc, nor)
FUSED_FUNC (eqv,  nor)
FUSED_FUNC (nand, nor)
FUSED_FUNC (nor,  nor)
FUSED_FUNC (or,   nor)
FUSED_FUNC (orc,  nor)
FUSED_FUNC (xor,  nor)

FUSED_FUNC (and,  or)
FUSED_FUNC (andc, or)
FUSED_FUNC (eqv,  or)
FUSED_FUNC (nand, or)
FUSED_FUNC (nor,  or)
FUSED_FUNC (or,   or)
FUSED_FUNC (orc,  or)
FUSED_FUNC (xor,  or)

FUSED_FUNC (and,  orc)
FUSED_FUNC (andc, orc)
FUSED_FUNC (eqv,  orc)
FUSED_FUNC (nand, orc)
FUSED_FUNC (nor,  orc)
FUSED_FUNC (or,   orc)
FUSED_FUNC (orc,  orc)
FUSED_FUNC (xor,  orc)

FUSED_FUNC (and,  xor)
FUSED_FUNC (andc, xor)
FUSED_FUNC (eqv,  xor)
FUSED_FUNC (nand, xor)
FUSED_FUNC (nor,  xor)
FUSED_FUNC (or,   xor)
FUSED_FUNC (orc,  xor)
FUSED_FUNC (xor,  xor)


/* List of functions to check.  */
typedef void func_t (vector_t,
		     vector_t,
		     vector_t);

typedef func_t *ptr_func_t;

static ptr_func_t functions[] = {
  and_and,
  andc_and,
  eqv_and,
  nand_and,
  nor_and,
  or_and,
  orc_and,
  xor_and,

  and_andc,
  andc_andc,
  eqv_andc,
  nand_andc,
  nor_andc,
  or_andc,
  orc_andc,
  xor_andc,

  and_eqv,
  andc_eqv,
  eqv_eqv,
  nand_eqv,
  nor_eqv,
  or_eqv,
  orc_eqv,
  xor_eqv,

  and_nand,
  andc_nand,
  eqv_nand,
  nand_nand,
  nor_nand,
  or_nand,
  orc_nand,
  xor_nand,

  and_nor,
  andc_nor,
  eqv_nor,
  nand_nor,
  nor_nor,
  or_nor,
  orc_nor,
  xor_nor,

  and_or,
  andc_or,
  eqv_or,
  nand_or,
  nor_or,
  or_or,
  orc_or,
  xor_or,

  and_orc,
  andc_orc,
  eqv_orc,
  nand_orc,
  nor_orc,
  or_orc,
  orc_orc,
  xor_orc,

  and_xor,
  andc_xor,
  eqv_xor,
  nand_xor,
  nor_xor,
  or_xor,
  orc_xor,
  xor_xor,
};


int
main (void)
{
  scalar_t s_a = 0x0fu;
  scalar_t s_b = 0xaau;
  scalar_t s_c = 0xccu;

  vector_t a = (vector_t) { s_a,  s_a, ~s_a, ~s_a };
  vector_t b = (vector_t) { s_b, ~s_b,  s_b, ~s_b };
  vector_t c = (vector_t) { s_c, ~s_c, ~s_c,  s_c };

  size_t i;

  for (i = 0; i < sizeof (functions) / sizeof (functions[0]); i++)
    functions[i] (a, b, c);

#ifdef DEBUG
  printf ("Done, %d tests, %d failures\n", tests, errors);
  return errors;

#else
  return 0;
#endif
}
