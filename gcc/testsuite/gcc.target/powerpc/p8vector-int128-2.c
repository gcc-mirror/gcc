/* { dg-do run } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

#include <stddef.h>
#include <stdlib.h>
#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>
#define UNUSED

#ifdef __LITTLE_ENDIAN__
#define HI_WORD 1
#define LO_WORD 0
#else
#define HI_WORD 0
#define LO_WORD 1
#endif

#else
#define UNUSED __attribute__((__unused__))
#endif

#ifndef S_TYPE
#define S_TYPE __uint128_t
#endif

#ifndef V_TYPE
#define V_TYPE vector S_TYPE
#endif

static int compare (S_TYPE, V_TYPE, const char *, const char *)
  __attribute__((__noinline__));

static int
compare (S_TYPE scalar,
	 V_TYPE vect,
	 const char *nl    UNUSED,
	 const char *which UNUSED)
{
  unsigned long scalar_lo = (unsigned long) scalar;
  unsigned long scalar_hi = (unsigned long) (scalar >> 64);
  unsigned long vect_lo;
  unsigned long vect_hi;
  vector long long tmp;
  int ret;

  __asm__ ("mfvsrd %0,%x3\n\t"
	   "xxpermdi %x2,%x3,%x3,3\n\t"
	   "mfvsrd %1,%x2"
	   : "=r" (vect_hi),
	     "=r" (vect_lo),
	     "=wa" (tmp)
	   : "wa" (vect));

  ret = (scalar_lo != vect_lo) || (scalar_hi != vect_hi);

#ifdef DEBUG
  printf ("%s%s: 0x%.16lx %.16lx %s 0x%.16lx %.16lx\n",
	  nl, which,
	  scalar_hi, scalar_lo,
	  (ret) ? "!=" : "==",
	  vect_hi, vect_lo);

  fflush (stdout);
#endif

  return ret;
}

static void convert_via_mem (V_TYPE *, S_TYPE *)
  __attribute__((__noinline__));

static void
convert_via_mem (V_TYPE *v, S_TYPE *s)
{
  *v = (V_TYPE) { *s };
  __asm__ volatile ("nop"
		    : "+m" (*s), "+m" (*v)
		    :
		    : "memory");

}


/* Check if vadduqm returns the same values as normal 128-bit add.  */

/* Values to add together.  */
const static struct {
  unsigned long hi_1;
  unsigned long lo_1;
  unsigned long hi_2;
  unsigned long lo_2;
} values[] = {
  { 0x0000000000000000UL, 0xfffffffffffffffeUL,
    0x0000000000000000UL, 0x0000000000000002UL },
  { 0x0000000000000000UL, 0x0000000000000002UL,
    0x0000000000000000UL, 0xfffffffffffffffeUL },
  { 0xffffffffffffffffUL, 0xfffffffffffffffeUL,
    0x0000000000000000UL, 0x0000000000000002UL },
  { 0xfffffffffffffff2UL, 0xffffffffffffffffUL,
    0x0000000000000002UL, 0x0000000000000000UL },
  { 0x7fffffffffffffffUL, 0xfffffffffffffffeUL,
    0x0000000000000000UL, 0x0000000000000002UL },
  { 0x7ffffffffffffff2UL, 0xffffffffffffffffUL,
    0x0000000000000002UL, 0x0000000000000000UL },
};

int
main (void)
{
  int reg_errors = 0;
  int mem_errors = 0;
  size_t i;
  const char *nl = "";

  for (i = 0; i < sizeof (values) / sizeof (values[0]); i++)
    {
      S_TYPE s_reg_res, s_reg_in1, s_reg_in2, s_mem_res, s_mem_in1, s_mem_in2;
      V_TYPE v_reg_res, v_reg_in1, v_reg_in2, v_mem_res, v_mem_in1, v_mem_in2;

      s_reg_in1 = ((((S_TYPE)values[i].hi_1 << 64)) + ((S_TYPE)values[i].lo_1));
      reg_errors += compare (s_reg_in1, (V_TYPE) { s_reg_in1 }, nl, "reg, in1");

      s_reg_in2 = ((((S_TYPE)values[i].hi_2 << 64)) + ((S_TYPE)values[i].lo_2));
      reg_errors += compare (s_reg_in2, (V_TYPE) { s_reg_in2 }, "", "reg, in2");

      s_reg_res = s_reg_in1 + s_reg_in2;

      v_reg_in1 = (V_TYPE) { s_reg_in1 };
      v_reg_in2 = (V_TYPE) { s_reg_in2 };
      v_reg_res = vec_vadduqm (v_reg_in1, v_reg_in2);
      reg_errors += compare (s_reg_res, v_reg_res, "", "reg, res");

      s_mem_in1 = s_reg_in1;
      convert_via_mem (&v_mem_in1, &s_mem_in1);
      mem_errors += compare (s_mem_in1, (V_TYPE) { s_mem_in1 }, "\n", "mem, in1");

      s_mem_in2 = s_reg_in2;
      convert_via_mem (&v_mem_in2, &s_mem_in2);
      mem_errors += compare (s_mem_in2, (V_TYPE) { s_mem_in2 }, "", "mem, in2");

      s_mem_res = s_mem_in1 + s_mem_in2;
      v_mem_res = vec_vadduqm (v_mem_in1, v_mem_in2);
      mem_errors += compare (s_mem_res, v_mem_res, "", "mem, res");

      nl = "\n";
    }

#ifdef DEBUG
  putchar ('\n');

  if (!reg_errors)
    fputs ("no errors found on register operations\n", stdout);
  else
    printf ("%d error%s found on register operations\n",
	    reg_errors,
	    (reg_errors == 1) ? "s" : "");

  if (!mem_errors)
    fputs ("no errors found on memory operations\n", stdout);
  else
    printf ("%d error%s found on memory operations\n",
	    mem_errors,
	    (mem_errors == 1) ? "s" : "");

  fflush (stdout);
#endif

  if ((reg_errors + mem_errors) != 0)
    abort ();

  return 0;
}
