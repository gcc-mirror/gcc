/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

void sha3hash (__dmr2048* dst)
{
  __builtin_dmsha3hash (dst, 31);
}

void sha3dw (__dmr2048* dst)
{
  __builtin_dmsha3dw (dst);
}

void cryshash (__dmr2048* dst)
{
  __builtin_dmcryshash (dst);
}

/* { dg-final { scan-assembler-times {\mlxvp\M} 24 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 24 } } */
/* { dg-final { scan-assembler-times {\mdmxxinstdmr512\M} 12 } } */
/* { dg-final { scan-assembler-times {\mdmxxextfdmr512\M} 12 } } */

/* { dg-final { scan-assembler-times {\mdmsha3hash\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmsha3dw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmcryshash\M} 1 } } */
