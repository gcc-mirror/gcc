/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */
/* { dg-require-effective-target powerpc_future_compile_ok } */

/* Test the ECC (Elliptic Curve Cryptography) acceleration builtins for Power future.
   These instructions support P-256 and P-384 elliptic curve operations. */

#include <altivec.h>

/* Test xxmulmul - Multiply-Multiply with scaling */
vector unsigned __int128
test_xxmulmul (vector unsigned long long a, vector unsigned long long b)
{
  return __builtin_vsx_xxmulmul (a, b, 3);
}

/* Test xxmulmulhiadd - Multiply-Multiply with high add and accumulator */
vector unsigned __int128
test_xxmulmulhiadd (vector unsigned __int128 acc,
		    vector unsigned long long a,
		    vector unsigned long long b)
{
  return __builtin_vsx_xxmulmulhiadd (acc, a, b, 1, 0, 1);
}

/* Test xxmulmulloadd - Multiply-Multiply low add with accumulator */
vector unsigned __int128
test_xxmulmulloadd (vector unsigned __int128 acc,
		    vector unsigned long long a,
		    vector unsigned long long b)
{
  return __builtin_vsx_xxmulmulloadd (acc, a, b, 1, 0);
}

/* Test xxssumudm - Scaled sum unsigned doubleword modulo */
vector unsigned __int128
test_xxssumudm (vector unsigned __int128 acc,
		vector unsigned long long a,
		vector unsigned long long b)
{
  return __builtin_vsx_xxssumudm (acc, a, b, 1);
}

/* Test xxssumudmc - Scaled sum unsigned doubleword modulo carry */
vector unsigned __int128
test_xxssumudmc (vector unsigned __int128 acc,
		 vector unsigned long long a,
		 vector unsigned long long b)
{
  return __builtin_vsx_xxssumudmc (acc, a, b, 0);
}

/* Test xxssumudmcext - Scaled sum unsigned doubleword modulo carry extended */
vector unsigned __int128
test_xxssumudmcext (vector unsigned long long a,
                    vector unsigned long long b,
                    vector unsigned __int128 c)
{
  return __builtin_vsx_xxssumudmcext (a, b, c, 1);
}

/* Test xsaddadduqm - Add add unsigned quadword modulo */
vector unsigned __int128
test_xsaddadduqm (vector unsigned __int128 acc,
                  vector unsigned __int128 a,
                  vector unsigned __int128 b)
{
  return __builtin_vsx_xsaddadduqm (acc, a, b);
}

/* Test xsaddaddsuqm - Add add scaled unsigned quadword modulo */
vector unsigned __int128
test_xsaddaddsuqm (vector unsigned __int128 acc,
                   vector unsigned __int128 a,
                   vector unsigned __int128 b)
{
  return __builtin_vsx_xsaddaddsuqm (acc, a, b);
}

/* Test xsaddsubuqm - Add subtract unsigned quadword modulo */
vector unsigned __int128
test_xsaddsubuqm (vector unsigned __int128 acc,
                  vector unsigned __int128 a,
                  vector unsigned __int128 b)
{
  return __builtin_vsx_xsaddsubuqm (acc, a, b);
}

/* Test xsaddsubsuqm - Add subtract scaled unsigned quadword modulo */
vector unsigned __int128
test_xsaddsubsuqm (vector unsigned __int128 acc,
                   vector unsigned __int128 a,
                   vector unsigned __int128 b)
{
  return __builtin_vsx_xsaddsubsuqm (acc, a, b);
}

/* Test xsmerge2t1uqm - Merge type 1 (2-operand) */
vector unsigned __int128
test_xsmerge2t1uqm (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return __builtin_vsx_xsmerge2t1uqm (a, b);
}

/* Test xsmerge2t2uqm - Merge type 2 (2-operand) */
vector unsigned __int128
test_xsmerge2t2uqm (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return __builtin_vsx_xsmerge2t2uqm (a, b);
}

/* Test xsmerge2t3uqm - Merge type 3 (2-operand) */
vector unsigned __int128
test_xsmerge2t3uqm (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return __builtin_vsx_xsmerge2t3uqm (a, b);
}

/* Test xsmerge3t1uqm - Merge type 1 (3-operand with accumulator) */
vector unsigned __int128
test_xsmerge3t1uqm (vector unsigned __int128 acc,
                    vector unsigned __int128 a,
                    vector unsigned __int128 b)
{
  return __builtin_vsx_xsmerge3t1uqm (acc, a, b);
}

/* Test xsrebase2t1uqm - Rebase type 1 (2-operand) */
vector unsigned __int128
test_xsrebase2t1uqm (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return __builtin_vsx_xsrebase2t1uqm (a, b);
}

/* Test xsrebase2t2uqm - Rebase type 2 (2-operand) */
vector unsigned __int128
test_xsrebase2t2uqm (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return __builtin_vsx_xsrebase2t2uqm (a, b);
}

/* Test xsrebase2t3uqm - Rebase type 3 (2-operand) */
vector unsigned __int128
test_xsrebase2t3uqm (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return __builtin_vsx_xsrebase2t3uqm (a, b);
}

/* Test xsrebase2t4uqm - Rebase type 4 (2-operand) */
vector unsigned __int128
test_xsrebase2t4uqm (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return __builtin_vsx_xsrebase2t4uqm (a, b);
}

/* Test xsrebase3t1uqm - Rebase type 1 (3-operand with accumulator) */
vector unsigned __int128
test_xsrebase3t1uqm (vector unsigned __int128 acc,
                     vector unsigned __int128 a,
                     vector unsigned __int128 b)
{
  return __builtin_vsx_xsrebase3t1uqm (acc, a, b);
}

/* Test xsrebase3t2uqm - Rebase type 2 (3-operand with accumulator) */
vector unsigned __int128
test_xsrebase3t2uqm (vector unsigned __int128 acc,
                     vector unsigned __int128 a,
                     vector unsigned __int128 b)
{
  return __builtin_vsx_xsrebase3t2uqm (acc, a, b);
}

/* Test xsrebase3t3uqm - Rebase type 3 (3-operand with accumulator) */
vector unsigned __int128
test_xsrebase3t3uqm (vector unsigned __int128 acc,
                     vector unsigned __int128 a,
                     vector unsigned __int128 b)
{
  return __builtin_vsx_xsrebase3t3uqm (acc, a, b);
}

/* { dg-final { scan-assembler-times {\mxxmulmul\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxmulmulhiadd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxmulmulloadd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxssumudm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxssumudmc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxssumudmcext\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsaddadduqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsaddaddsuqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsaddsubuqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsaddsubsuqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsmerge2t1uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsmerge2t2uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsmerge2t3uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsmerge3t1uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsrebase2t1uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsrebase2t2uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsrebase2t3uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsrebase2t4uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsrebase3t1uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsrebase3t2uqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsrebase3t3uqm\M} 1 } } */
