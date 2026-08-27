/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

void
sha2hash_256 (__dmr1024 *dst, __dmr1024 *src)
{
  __builtin_dmsha2hash (dst, src, 0);
}

void
sha2hash_512 (__dmr1024 *dst, __dmr1024 *src)
{
  __builtin_dmsha2hash (dst, src, 1);
}

void
sha256hash (__dmr1024 *dst, __dmr1024 *src)
{
  __builtin_dmsha256hash (dst, src);
}

void
sha512hash (__dmr1024 *dst, __dmr1024 *src)
{
  __builtin_dmsha512hash (dst, src);
}

/* Each function loads both dst and src. src is loaded since it is an input to
   the instruction. dst is loaded to preserve the state of hashing.  */
/* { dg-final { scan-assembler-times {\mlxvp\M} 32 } } */

/* Each function stores only one dmr into dst.  */
/* { dg-final { scan-assembler-times {\mstxvp\M} 16 } } */

/* { dg-final { scan-assembler-times {\mdmsha2hash\M} 2 } } */
/* { dg-final { scan-assembler-times {\mdmsha2hash\M\s+[^,\n]+,\s*[^,\n]+,\s*0\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmsha2hash\M\s+[^,\n]+,\s*[^,\n]+,\s*1\M} 1 } } */

/* { dg-final { scan-assembler-times {\mdmsha256hash\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmsha512hash\M} 1 } } */
