/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
shapad_0_0_0 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 0, 0, 0);
}
void
shapad_0_0_1 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 0, 0, 1);
}
void
shapad_0_0_2 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 0, 0, 2);
}
void
shapad_0_0_3 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 0, 0, 3);
}
void
shapad_0_1_0 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 0, 1, 0);
}
void
shapad_0_1_1 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 0, 1, 1);
}
void
shapad_0_1_2 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 0, 1, 2);
}
void
shapad_0_1_3 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 0, 1, 3);
}

void
shapad_1_0_2 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 1, 0, 2);
}
void
shapad_1_0_3 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 1, 0, 3);
}
void
shapad_1_1_2 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 1, 1, 2);
}
void
shapad_1_1_3 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 1, 1, 3);
}

void
shapad_2_0_0 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 2, 0, 0);
}
void
shapad_2_0_1 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 2, 0, 1);
}
void
shapad_2_0_2 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 2, 0, 2);
}
void
shapad_2_0_3 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 2, 0, 3);
}
void
shapad_2_1_0 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 2, 1, 0);
}
void
shapad_2_1_1 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 2, 1, 1);
}
void
shapad_2_1_2 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 2, 1, 2);
}
void
shapad_2_1_3 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 2, 1, 3);
}

void
shapad_3_0_0 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 3, 0, 0);
}
void
shapad_3_0_1 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 3, 0, 1);
}
void
shapad_3_0_2 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 3, 0, 2);
}
void
shapad_3_0_3 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 3, 0, 3);
}
void
shapad_3_1_0 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 3, 1, 0);
}
void
shapad_3_1_1 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 3, 1, 1);
}
void
shapad_3_1_2 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 3, 1, 2);
}
void
shapad_3_1_3 (__dmr1024 *res, vec_t pd)
{
  __builtin_dmxxshapad (res, pd, 3, 1, 3);
}

/* Each function loads dst into dmr to preserve state of padding.  */
/* { dg-final { scan-assembler-times {\mlxvp\M} 112 } } */

/* Each function stores only one dmr into dst.  */
/* { dg-final { scan-assembler-times {\mstxvp\M} 112 } } */

/* { dg-final { scan-assembler-times {\mdmxxshapad\M} 28 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*0,\s*0,\s*0\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*0,\s*0,\s*1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*0,\s*0,\s*2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*0,\s*0,\s*3\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*0,\s*1,\s*0\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*0,\s*1,\s*1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*0,\s*1,\s*2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*0,\s*1,\s*3\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*1,\s*0,\s*2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*1,\s*0,\s*3\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*1,\s*1,\s*2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*1,\s*1,\s*3\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*2,\s*0,\s*0\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*2,\s*0,\s*1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*2,\s*0,\s*2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*2,\s*0,\s*3\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*2,\s*1,\s*0\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*2,\s*1,\s*1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*2,\s*1,\s*2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*2,\s*1,\s*3\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*3,\s*0,\s*0\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*3,\s*0,\s*1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*3,\s*0,\s*2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*3,\s*0,\s*3\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*3,\s*1,\s*0\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*3,\s*1,\s*1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*3,\s*1,\s*2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdmxxshapad\M\s+[^,\n]+,\s*[^,\n],\s*3,\s*1,\s*3\M} 1 } } */
