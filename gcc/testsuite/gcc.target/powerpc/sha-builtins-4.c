/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void sha3512pad (__dmr1024* dst, vec_t pd)
{
  __builtin_dmxxsha3512pad (dst, pd, 0);
}
void sha3384pad (__dmr1024* dst, vec_t pd)
{
  __builtin_dmxxsha3384pad (dst, pd, 0);
}
void sha3256pad (__dmr1024* dst, vec_t pd)
{
  __builtin_dmxxsha3256pad (dst, pd, 0);
}
void sha3224pad (__dmr1024* dst, vec_t pd)
{
  __builtin_dmxxsha3224pad (dst, pd, 0);
}
void shake256pad (__dmr1024* dst, vec_t pd)
{
  __builtin_dmxxshake256pad (dst, pd, 0);
}
void shake128pad (__dmr1024* dst, vec_t pd)
{
  __builtin_dmxxshake128pad (dst, pd, 0);
}
void sha384512pad (__dmr1024* dst, vec_t pd)
{
  __builtin_dmxxsha384512pad (dst, pd);
}
void sha224256pad (__dmr1024* dst, vec_t pd)
{
  __builtin_dmxxsha224256pad (dst, pd);
}

/* Each function loads dst into dmr to preserve state of padding.  */
/* { dg-final { scan-assembler-times {\mlxvp\M} 32 } } */

/* Each function stores only one dmr into dst.  */
/* { dg-final { scan-assembler-times {\mstxvp\M} 32 } } */

/* { dg-final { scan-assembler-times {\mdmxxsha3512pad\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxsha3384pad\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxsha3256pad\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxsha3224pad\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshake256pad\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshake128pad\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxsha384512pad\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxsha224256pad\M} 1 } } */
