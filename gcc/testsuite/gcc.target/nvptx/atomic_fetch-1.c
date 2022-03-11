/* Test the nvptx atomic instructions for __atomic_fetch_OP for SM_35
   targets.  */

/* { dg-do compile } */
/* { dg-options "-O2 -misa=sm_35" } */

enum memmodel
{
  MEMMODEL_RELAXED = 0
};

unsigned long long int *p64;
unsigned int *p32;

unsigned long long int g64;
unsigned int g32;

unsigned int s32 __attribute__((shared));
unsigned long long int s64 __attribute__((shared));

unsigned long long int v64;
unsigned int v32;

int
main()
{
  /* Generic.  */

  __atomic_fetch_add (p64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_and (p64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_or (p64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_xor (p64, v64, MEMMODEL_RELAXED);
  
  __atomic_fetch_add (p32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_and (p32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_or (p32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_xor (p32, v32, MEMMODEL_RELAXED);

  /* Global.  */

  __atomic_fetch_add (&g64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_and (&g64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_or (&g64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_xor (&g64, v64, MEMMODEL_RELAXED);
  
  __atomic_fetch_add (&g32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_and (&g32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_or (&g32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_xor (&g32, v32, MEMMODEL_RELAXED);

  /* Shared.  */

  __atomic_fetch_add (&s64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_and (&s64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_or (&s64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_xor (&s64, v64, MEMMODEL_RELAXED);
  
  __atomic_fetch_add (&s32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_and (&s32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_or (&s32, v32, MEMMODEL_RELAXED);
  __atomic_fetch_xor (&s32, v32, MEMMODEL_RELAXED);

  return 0;
}

/* Generic.  */

/* { dg-final { scan-assembler-times "atom.add.u64" 1 } } */
/* { dg-final { scan-assembler-times "atom.and.b64" 1 } } */
/* { dg-final { scan-assembler-times "atom.or.b64" 1 } } */
/* { dg-final { scan-assembler-times "atom.xor.b64" 1 } } */

/* { dg-final { scan-assembler-times "atom.add.u32" 1 } } */
/* { dg-final { scan-assembler-times "atom.and.b32" 1 } } */
/* { dg-final { scan-assembler-times "atom.or.b32" 1 } } */
/* { dg-final { scan-assembler-times "atom.xor.b32" 1 } } */

/* Global.  */

/* { dg-final { scan-assembler-times "atom.global.add.u64" 1 } } */
/* { dg-final { scan-assembler-times "atom.global.and.b64" 1 } } */
/* { dg-final { scan-assembler-times "atom.global.or.b64" 1 } } */
/* { dg-final { scan-assembler-times "atom.global.xor.b64" 1 } } */

/* { dg-final { scan-assembler-times "atom.global.add.u32" 1 } } */
/* { dg-final { scan-assembler-times "atom.global.and.b32" 1 } } */
/* { dg-final { scan-assembler-times "atom.global.or.b32" 1 } } */
/* { dg-final { scan-assembler-times "atom.global.xor.b32" 1 } } */

/* Shared.  */

/* { dg-final { scan-assembler-times "atom.shared.add.u64" 1 } } */
/* { dg-final { scan-assembler-times "atom.shared.and.b64" 1 } } */
/* { dg-final { scan-assembler-times "atom.shared.or.b64" 1 } } */
/* { dg-final { scan-assembler-times "atom.shared.xor.b64" 1 } } */

/* { dg-final { scan-assembler-times "atom.shared.add.u32" 1 } } */
/* { dg-final { scan-assembler-times "atom.shared.and.b32" 1 } } */
/* { dg-final { scan-assembler-times "atom.shared.or.b32" 1 } } */
/* { dg-final { scan-assembler-times "atom.shared.xor.b32" 1 } } */
