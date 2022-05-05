/* Test the atomic exchange expansion, global state space.  */

/* { dg-do compile } */
/* { dg-options "-Wno-long-long" } */

enum memmodel
{
  MEMMODEL_SEQ_CST = 5
};

unsigned char u8;
unsigned short u16;
unsigned int u32;
unsigned long long int u64;

int
main()
{
  __atomic_exchange_n (&u8, 0, MEMMODEL_SEQ_CST);
  __atomic_exchange_n (&u16, 0, MEMMODEL_SEQ_CST);
  __atomic_exchange_n (&u32, 0, MEMMODEL_SEQ_CST);
  __atomic_exchange_n (&u64, 0, MEMMODEL_SEQ_CST);

  return 0;
}

/* Not ptx-native, fallback to libatomic.  */
/* { dg-final { scan-assembler-times "(?n)call .* __atomic_exchange_1" 1 } } */
/* { dg-final { scan-assembler-times "(?n)call .* __atomic_exchange_2" 1 } } */

/* { dg-final { scan-assembler-times "atom.global.exch.b32" 1 } } */
/* { dg-final { scan-assembler-times "atom.global.exch.b64" 1 } } */
/* { dg-final { scan-assembler-times "membar.sys" 4 } } */
