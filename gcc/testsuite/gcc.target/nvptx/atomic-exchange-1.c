/* Test the atomic exchange expansion, shared state space.  */

/* { dg-do compile } */
/* { dg-options "-Wno-long-long" } */

enum memmodel
{
  MEMMODEL_SEQ_CST = 5
};

unsigned char u8 __attribute__((shared));
unsigned short u16 __attribute__((shared));
unsigned int u32 __attribute__((shared));
unsigned long long int u64 __attribute__((shared));

int
main()
{
  __atomic_exchange_n (&u8, 0, MEMMODEL_SEQ_CST);
  __atomic_exchange_n (&u16, 0, MEMMODEL_SEQ_CST);
  __atomic_exchange_n (&u32, 0, MEMMODEL_SEQ_CST);
  __atomic_exchange_n (&u64, 0, MEMMODEL_SEQ_CST);

  return 0;
}


/* Not ptx-native, fallback to libatomic.
   Libatomic uses generic addressing with a global lock and membar.sys barriers.
   We could implement these more efficiently by cloning libatomic for .shared,
   using a per-CTA lock and membar.cta barrier.  But we'd expect
   performance-critical code to use the ptx-native atomic sizes 32 and 64 bit,
   so that doesn't seem to be worth the trouble.  */
/* { dg-final { scan-assembler-times "(?n)call .* __atomic_exchange_1" 1 } } */
/* { dg-final { scan-assembler-times "(?n)call .* __atomic_exchange_2" 1 } } */

/* { dg-final { scan-assembler-times "atom.shared.exch.b32" 1 } } */
/* { dg-final { scan-assembler-times "atom.shared.exch.b64" 1 } } */
/* { dg-final { scan-assembler-times "membar.cta" 4 } } */
