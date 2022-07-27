/* Test the atomic store expansion, global state space.  */

/* { dg-do compile } */
/* { dg-additional-options "-Wno-long-long -misa=sm_30" } */

enum memmodel
{
  MEMMODEL_SEQ_CST = 5
};

unsigned int u32;
unsigned long long int u64;

int
main()
{
  __atomic_store_n (&u32, 0, MEMMODEL_SEQ_CST);
  __atomic_store_n (&u64, 0, MEMMODEL_SEQ_CST);

  return 0;
}

/* { dg-final { scan-assembler-times "st.global.u32" 1 } } */
/* { dg-final { scan-assembler-times "st.global.u64" 1 } } */
/* { dg-final { scan-assembler-times "membar.sys" 4 } } */
