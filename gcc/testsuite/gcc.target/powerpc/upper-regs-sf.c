/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -mupper-regs-df -mupper-regs-sf" } */

/* Test for the -mupper-regs-df option to make sure double values are allocated
   to the Altivec registers as well as the traditional FPR registers.  */

#ifndef TYPE
#define TYPE float
#endif

#ifndef MASK_TYPE
#define MASK_TYPE unsigned long long
#endif

#define MASK_ONE	((MASK_TYPE)1)
#define ZERO		((TYPE) 0.0)

TYPE
test_add (const MASK_TYPE *add_mask, const TYPE *add_values,
	  const MASK_TYPE *sub_mask, const TYPE *sub_values,
	  const MASK_TYPE *mul_mask, const TYPE *mul_values,
	  const MASK_TYPE *div_mask, const TYPE *div_values,
	  const MASK_TYPE *eq0_mask, int *eq0_ptr)
{
  TYPE value;
  TYPE value00	= ZERO;
  TYPE value01	= ZERO;
  TYPE value02	= ZERO;
  TYPE value03	= ZERO;
  TYPE value04	= ZERO;
  TYPE value05	= ZERO;
  TYPE value06	= ZERO;
  TYPE value07	= ZERO;
  TYPE value08	= ZERO;
  TYPE value09	= ZERO;
  TYPE value10	= ZERO;
  TYPE value11	= ZERO;
  TYPE value12	= ZERO;
  TYPE value13	= ZERO;
  TYPE value14	= ZERO;
  TYPE value15	= ZERO;
  TYPE value16	= ZERO;
  TYPE value17	= ZERO;
  TYPE value18	= ZERO;
  TYPE value19	= ZERO;
  TYPE value20	= ZERO;
  TYPE value21	= ZERO;
  TYPE value22	= ZERO;
  TYPE value23	= ZERO;
  TYPE value24	= ZERO;
  TYPE value25	= ZERO;
  TYPE value26	= ZERO;
  TYPE value27	= ZERO;
  TYPE value28	= ZERO;
  TYPE value29	= ZERO;
  TYPE value30	= ZERO;
  TYPE value31	= ZERO;
  TYPE value32	= ZERO;
  TYPE value33	= ZERO;
  TYPE value34	= ZERO;
  TYPE value35	= ZERO;
  TYPE value36	= ZERO;
  TYPE value37	= ZERO;
  TYPE value38	= ZERO;
  TYPE value39	= ZERO;
  MASK_TYPE mask;
  int eq0;

  while ((mask = *add_mask++) != 0)
    {
      value = *add_values++;

      __asm__ (" #reg %0" : "+d" (value));

      if ((mask & (MASK_ONE <<  0)) != 0)
	value00 += value;

      if ((mask & (MASK_ONE <<  1)) != 0)
	value01 += value;

      if ((mask & (MASK_ONE <<  2)) != 0)
	value02 += value;

      if ((mask & (MASK_ONE <<  3)) != 0)
	value03 += value;

      if ((mask & (MASK_ONE <<  4)) != 0)
	value04 += value;

      if ((mask & (MASK_ONE <<  5)) != 0)
	value05 += value;

      if ((mask & (MASK_ONE <<  6)) != 0)
	value06 += value;

      if ((mask & (MASK_ONE <<  7)) != 0)
	value07 += value;

      if ((mask & (MASK_ONE <<  8)) != 0)
	value08 += value;

      if ((mask & (MASK_ONE <<  9)) != 0)
	value09 += value;

      if ((mask & (MASK_ONE << 10)) != 0)
	value10 += value;

      if ((mask & (MASK_ONE << 11)) != 0)
	value11 += value;

      if ((mask & (MASK_ONE << 12)) != 0)
	value12 += value;

      if ((mask & (MASK_ONE << 13)) != 0)
	value13 += value;

      if ((mask & (MASK_ONE << 14)) != 0)
	value14 += value;

      if ((mask & (MASK_ONE << 15)) != 0)
	value15 += value;

      if ((mask & (MASK_ONE << 16)) != 0)
	value16 += value;

      if ((mask & (MASK_ONE << 17)) != 0)
	value17 += value;

      if ((mask & (MASK_ONE << 18)) != 0)
	value18 += value;

      if ((mask & (MASK_ONE << 19)) != 0)
	value19 += value;

      if ((mask & (MASK_ONE << 20)) != 0)
	value20 += value;

      if ((mask & (MASK_ONE << 21)) != 0)
	value21 += value;

      if ((mask & (MASK_ONE << 22)) != 0)
	value22 += value;

      if ((mask & (MASK_ONE << 23)) != 0)
	value23 += value;

      if ((mask & (MASK_ONE << 24)) != 0)
	value24 += value;

      if ((mask & (MASK_ONE << 25)) != 0)
	value25 += value;

      if ((mask & (MASK_ONE << 26)) != 0)
	value26 += value;

      if ((mask & (MASK_ONE << 27)) != 0)
	value27 += value;

      if ((mask & (MASK_ONE << 28)) != 0)
	value28 += value;

      if ((mask & (MASK_ONE << 29)) != 0)
	value29 += value;

      if ((mask & (MASK_ONE << 30)) != 0)
	value30 += value;

      if ((mask & (MASK_ONE << 31)) != 0)
	value31 += value;

      if ((mask & (MASK_ONE << 32)) != 0)
	value32 += value;

      if ((mask & (MASK_ONE << 33)) != 0)
	value33 += value;

      if ((mask & (MASK_ONE << 34)) != 0)
	value34 += value;

      if ((mask & (MASK_ONE << 35)) != 0)
	value35 += value;

      if ((mask & (MASK_ONE << 36)) != 0)
	value36 += value;

      if ((mask & (MASK_ONE << 37)) != 0)
	value37 += value;

      if ((mask & (MASK_ONE << 38)) != 0)
	value38 += value;

      if ((mask & (MASK_ONE << 39)) != 0)
	value39 += value;
    }

  while ((mask = *sub_mask++) != 0)
    {
      value = *sub_values++;

      __asm__ (" #reg %0" : "+d" (value));

      if ((mask & (MASK_ONE <<  0)) != 0)
	value00 -= value;

      if ((mask & (MASK_ONE <<  1)) != 0)
	value01 -= value;

      if ((mask & (MASK_ONE <<  2)) != 0)
	value02 -= value;

      if ((mask & (MASK_ONE <<  3)) != 0)
	value03 -= value;

      if ((mask & (MASK_ONE <<  4)) != 0)
	value04 -= value;

      if ((mask & (MASK_ONE <<  5)) != 0)
	value05 -= value;

      if ((mask & (MASK_ONE <<  6)) != 0)
	value06 -= value;

      if ((mask & (MASK_ONE <<  7)) != 0)
	value07 -= value;

      if ((mask & (MASK_ONE <<  8)) != 0)
	value08 -= value;

      if ((mask & (MASK_ONE <<  9)) != 0)
	value09 -= value;

      if ((mask & (MASK_ONE << 10)) != 0)
	value10 -= value;

      if ((mask & (MASK_ONE << 11)) != 0)
	value11 -= value;

      if ((mask & (MASK_ONE << 12)) != 0)
	value12 -= value;

      if ((mask & (MASK_ONE << 13)) != 0)
	value13 -= value;

      if ((mask & (MASK_ONE << 14)) != 0)
	value14 -= value;

      if ((mask & (MASK_ONE << 15)) != 0)
	value15 -= value;

      if ((mask & (MASK_ONE << 16)) != 0)
	value16 -= value;

      if ((mask & (MASK_ONE << 17)) != 0)
	value17 -= value;

      if ((mask & (MASK_ONE << 18)) != 0)
	value18 -= value;

      if ((mask & (MASK_ONE << 19)) != 0)
	value19 -= value;

      if ((mask & (MASK_ONE << 20)) != 0)
	value20 -= value;

      if ((mask & (MASK_ONE << 21)) != 0)
	value21 -= value;

      if ((mask & (MASK_ONE << 22)) != 0)
	value22 -= value;

      if ((mask & (MASK_ONE << 23)) != 0)
	value23 -= value;

      if ((mask & (MASK_ONE << 24)) != 0)
	value24 -= value;

      if ((mask & (MASK_ONE << 25)) != 0)
	value25 -= value;

      if ((mask & (MASK_ONE << 26)) != 0)
	value26 -= value;

      if ((mask & (MASK_ONE << 27)) != 0)
	value27 -= value;

      if ((mask & (MASK_ONE << 28)) != 0)
	value28 -= value;

      if ((mask & (MASK_ONE << 29)) != 0)
	value29 -= value;

      if ((mask & (MASK_ONE << 30)) != 0)
	value30 -= value;

      if ((mask & (MASK_ONE << 31)) != 0)
	value31 -= value;

      if ((mask & (MASK_ONE << 32)) != 0)
	value32 -= value;

      if ((mask & (MASK_ONE << 33)) != 0)
	value33 -= value;

      if ((mask & (MASK_ONE << 34)) != 0)
	value34 -= value;

      if ((mask & (MASK_ONE << 35)) != 0)
	value35 -= value;

      if ((mask & (MASK_ONE << 36)) != 0)
	value36 -= value;

      if ((mask & (MASK_ONE << 37)) != 0)
	value37 -= value;

      if ((mask & (MASK_ONE << 38)) != 0)
	value38 -= value;

      if ((mask & (MASK_ONE << 39)) != 0)
	value39 -= value;
    }

  while ((mask = *mul_mask++) != 0)
    {
      value = *mul_values++;

      __asm__ (" #reg %0" : "+d" (value));

      if ((mask & (MASK_ONE <<  0)) != 0)
	value00 *= value;

      if ((mask & (MASK_ONE <<  1)) != 0)
	value01 *= value;

      if ((mask & (MASK_ONE <<  2)) != 0)
	value02 *= value;

      if ((mask & (MASK_ONE <<  3)) != 0)
	value03 *= value;

      if ((mask & (MASK_ONE <<  4)) != 0)
	value04 *= value;

      if ((mask & (MASK_ONE <<  5)) != 0)
	value05 *= value;

      if ((mask & (MASK_ONE <<  6)) != 0)
	value06 *= value;

      if ((mask & (MASK_ONE <<  7)) != 0)
	value07 *= value;

      if ((mask & (MASK_ONE <<  8)) != 0)
	value08 *= value;

      if ((mask & (MASK_ONE <<  9)) != 0)
	value09 *= value;

      if ((mask & (MASK_ONE << 10)) != 0)
	value10 *= value;

      if ((mask & (MASK_ONE << 11)) != 0)
	value11 *= value;

      if ((mask & (MASK_ONE << 12)) != 0)
	value12 *= value;

      if ((mask & (MASK_ONE << 13)) != 0)
	value13 *= value;

      if ((mask & (MASK_ONE << 14)) != 0)
	value14 *= value;

      if ((mask & (MASK_ONE << 15)) != 0)
	value15 *= value;

      if ((mask & (MASK_ONE << 16)) != 0)
	value16 *= value;

      if ((mask & (MASK_ONE << 17)) != 0)
	value17 *= value;

      if ((mask & (MASK_ONE << 18)) != 0)
	value18 *= value;

      if ((mask & (MASK_ONE << 19)) != 0)
	value19 *= value;

      if ((mask & (MASK_ONE << 20)) != 0)
	value20 *= value;

      if ((mask & (MASK_ONE << 21)) != 0)
	value21 *= value;

      if ((mask & (MASK_ONE << 22)) != 0)
	value22 *= value;

      if ((mask & (MASK_ONE << 23)) != 0)
	value23 *= value;

      if ((mask & (MASK_ONE << 24)) != 0)
	value24 *= value;

      if ((mask & (MASK_ONE << 25)) != 0)
	value25 *= value;

      if ((mask & (MASK_ONE << 26)) != 0)
	value26 *= value;

      if ((mask & (MASK_ONE << 27)) != 0)
	value27 *= value;

      if ((mask & (MASK_ONE << 28)) != 0)
	value28 *= value;

      if ((mask & (MASK_ONE << 29)) != 0)
	value29 *= value;

      if ((mask & (MASK_ONE << 30)) != 0)
	value30 *= value;

      if ((mask & (MASK_ONE << 31)) != 0)
	value31 *= value;

      if ((mask & (MASK_ONE << 32)) != 0)
	value32 *= value;

      if ((mask & (MASK_ONE << 33)) != 0)
	value33 *= value;

      if ((mask & (MASK_ONE << 34)) != 0)
	value34 *= value;

      if ((mask & (MASK_ONE << 35)) != 0)
	value35 *= value;

      if ((mask & (MASK_ONE << 36)) != 0)
	value36 *= value;

      if ((mask & (MASK_ONE << 37)) != 0)
	value37 *= value;

      if ((mask & (MASK_ONE << 38)) != 0)
	value38 *= value;

      if ((mask & (MASK_ONE << 39)) != 0)
	value39 *= value;
    }

  while ((mask = *div_mask++) != 0)
    {
      value = *div_values++;

      __asm__ (" #reg %0" : "+d" (value));

      if ((mask & (MASK_ONE <<  0)) != 0)
	value00 /= value;

      if ((mask & (MASK_ONE <<  1)) != 0)
	value01 /= value;

      if ((mask & (MASK_ONE <<  2)) != 0)
	value02 /= value;

      if ((mask & (MASK_ONE <<  3)) != 0)
	value03 /= value;

      if ((mask & (MASK_ONE <<  4)) != 0)
	value04 /= value;

      if ((mask & (MASK_ONE <<  5)) != 0)
	value05 /= value;

      if ((mask & (MASK_ONE <<  6)) != 0)
	value06 /= value;

      if ((mask & (MASK_ONE <<  7)) != 0)
	value07 /= value;

      if ((mask & (MASK_ONE <<  8)) != 0)
	value08 /= value;

      if ((mask & (MASK_ONE <<  9)) != 0)
	value09 /= value;

      if ((mask & (MASK_ONE << 10)) != 0)
	value10 /= value;

      if ((mask & (MASK_ONE << 11)) != 0)
	value11 /= value;

      if ((mask & (MASK_ONE << 12)) != 0)
	value12 /= value;

      if ((mask & (MASK_ONE << 13)) != 0)
	value13 /= value;

      if ((mask & (MASK_ONE << 14)) != 0)
	value14 /= value;

      if ((mask & (MASK_ONE << 15)) != 0)
	value15 /= value;

      if ((mask & (MASK_ONE << 16)) != 0)
	value16 /= value;

      if ((mask & (MASK_ONE << 17)) != 0)
	value17 /= value;

      if ((mask & (MASK_ONE << 18)) != 0)
	value18 /= value;

      if ((mask & (MASK_ONE << 19)) != 0)
	value19 /= value;

      if ((mask & (MASK_ONE << 20)) != 0)
	value20 /= value;

      if ((mask & (MASK_ONE << 21)) != 0)
	value21 /= value;

      if ((mask & (MASK_ONE << 22)) != 0)
	value22 /= value;

      if ((mask & (MASK_ONE << 23)) != 0)
	value23 /= value;

      if ((mask & (MASK_ONE << 24)) != 0)
	value24 /= value;

      if ((mask & (MASK_ONE << 25)) != 0)
	value25 /= value;

      if ((mask & (MASK_ONE << 26)) != 0)
	value26 /= value;

      if ((mask & (MASK_ONE << 27)) != 0)
	value27 /= value;

      if ((mask & (MASK_ONE << 28)) != 0)
	value28 /= value;

      if ((mask & (MASK_ONE << 29)) != 0)
	value29 /= value;

      if ((mask & (MASK_ONE << 30)) != 0)
	value30 /= value;

      if ((mask & (MASK_ONE << 31)) != 0)
	value31 /= value;

      if ((mask & (MASK_ONE << 32)) != 0)
	value32 /= value;

      if ((mask & (MASK_ONE << 33)) != 0)
	value33 /= value;

      if ((mask & (MASK_ONE << 34)) != 0)
	value34 /= value;

      if ((mask & (MASK_ONE << 35)) != 0)
	value35 /= value;

      if ((mask & (MASK_ONE << 36)) != 0)
	value36 /= value;

      if ((mask & (MASK_ONE << 37)) != 0)
	value37 /= value;

      if ((mask & (MASK_ONE << 38)) != 0)
	value38 /= value;

      if ((mask & (MASK_ONE << 39)) != 0)
	value39 /= value;
    }

  while ((mask = *eq0_mask++) != 0)
    {
      eq0 = 0;

      if ((mask & (MASK_ONE <<  0)) != 0)
	eq0 |= (value00 == ZERO);

      if ((mask & (MASK_ONE <<  1)) != 0)
	eq0 |= (value01 == ZERO);

      if ((mask & (MASK_ONE <<  2)) != 0)
	eq0 |= (value02 == ZERO);

      if ((mask & (MASK_ONE <<  3)) != 0)
	eq0 |= (value03 == ZERO);

      if ((mask & (MASK_ONE <<  4)) != 0)
	eq0 |= (value04 == ZERO);

      if ((mask & (MASK_ONE <<  5)) != 0)
	eq0 |= (value05 == ZERO);

      if ((mask & (MASK_ONE <<  6)) != 0)
	eq0 |= (value06 == ZERO);

      if ((mask & (MASK_ONE <<  7)) != 0)
	eq0 |= (value07 == ZERO);

      if ((mask & (MASK_ONE <<  8)) != 0)
	eq0 |= (value08 == ZERO);

      if ((mask & (MASK_ONE <<  9)) != 0)
	eq0 |= (value09 == ZERO);

      if ((mask & (MASK_ONE << 10)) != 0)
	eq0 |= (value10 == ZERO);

      if ((mask & (MASK_ONE << 11)) != 0)
	eq0 |= (value11 == ZERO);

      if ((mask & (MASK_ONE << 12)) != 0)
	eq0 |= (value12 == ZERO);

      if ((mask & (MASK_ONE << 13)) != 0)
	eq0 |= (value13 == ZERO);

      if ((mask & (MASK_ONE << 14)) != 0)
	eq0 |= (value14 == ZERO);

      if ((mask & (MASK_ONE << 15)) != 0)
	eq0 |= (value15 == ZERO);

      if ((mask & (MASK_ONE << 16)) != 0)
	eq0 |= (value16 == ZERO);

      if ((mask & (MASK_ONE << 17)) != 0)
	eq0 |= (value17 == ZERO);

      if ((mask & (MASK_ONE << 18)) != 0)
	eq0 |= (value18 == ZERO);

      if ((mask & (MASK_ONE << 19)) != 0)
	eq0 |= (value19 == ZERO);

      if ((mask & (MASK_ONE << 20)) != 0)
	eq0 |= (value20 == ZERO);

      if ((mask & (MASK_ONE << 21)) != 0)
	eq0 |= (value21 == ZERO);

      if ((mask & (MASK_ONE << 22)) != 0)
	eq0 |= (value22 == ZERO);

      if ((mask & (MASK_ONE << 23)) != 0)
	eq0 |= (value23 == ZERO);

      if ((mask & (MASK_ONE << 24)) != 0)
	eq0 |= (value24 == ZERO);

      if ((mask & (MASK_ONE << 25)) != 0)
	eq0 |= (value25 == ZERO);

      if ((mask & (MASK_ONE << 26)) != 0)
	eq0 |= (value26 == ZERO);

      if ((mask & (MASK_ONE << 27)) != 0)
	eq0 |= (value27 == ZERO);

      if ((mask & (MASK_ONE << 28)) != 0)
	eq0 |= (value28 == ZERO);

      if ((mask & (MASK_ONE << 29)) != 0)
	eq0 |= (value29 == ZERO);

      if ((mask & (MASK_ONE << 30)) != 0)
	eq0 |= (value30 == ZERO);

      if ((mask & (MASK_ONE << 31)) != 0)
	eq0 |= (value31 == ZERO);

      if ((mask & (MASK_ONE << 32)) != 0)
	eq0 |= (value32 == ZERO);

      if ((mask & (MASK_ONE << 33)) != 0)
	eq0 |= (value33 == ZERO);

      if ((mask & (MASK_ONE << 34)) != 0)
	eq0 |= (value34 == ZERO);

      if ((mask & (MASK_ONE << 35)) != 0)
	eq0 |= (value35 == ZERO);

      if ((mask & (MASK_ONE << 36)) != 0)
	eq0 |= (value36 == ZERO);

      if ((mask & (MASK_ONE << 37)) != 0)
	eq0 |= (value37 == ZERO);

      if ((mask & (MASK_ONE << 38)) != 0)
	eq0 |= (value38 == ZERO);

      if ((mask & (MASK_ONE << 39)) != 0)
	eq0 |= (value39 == ZERO);

      *eq0_ptr++ = eq0;
    }

  return (  value00 + value01 + value02 + value03 + value04
	  + value05 + value06 + value07 + value08 + value09
	  + value10 + value11 + value12 + value13 + value14
	  + value15 + value16 + value17 + value18 + value19
	  + value20 + value21 + value22 + value23 + value24
	  + value25 + value26 + value27 + value28 + value29
	  + value30 + value31 + value32 + value33 + value34
	  + value35 + value36 + value37 + value38 + value39);
}

/* { dg-final { scan-assembler "fadds"    } } */
/* { dg-final { scan-assembler "fsubs"    } } */
/* { dg-final { scan-assembler "fmuls"    } } */
/* { dg-final { scan-assembler "fdivs"    } } */
/* { dg-final { scan-assembler "fcmpu"    } } */
/* { dg-final { scan-assembler "xsaddsp"  } } */
/* { dg-final { scan-assembler "xssubsp"  } } */
/* { dg-final { scan-assembler "xsmulsp"  } } */
/* { dg-final { scan-assembler "xsdivsp"  } } */
/* { dg-final { scan-assembler "xscmpudp" } } */
