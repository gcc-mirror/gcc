/* Test the atomic store expansion for sm > sm_6x targets,
   shared state space.  */

/* { dg-do compile } */
/* { dg-options "-misa=sm_70 -mptx=_" } */

enum memmodel
{
  MEMMODEL_SEQ_CST = 5
};

unsigned int u32 __attribute__((shared));
unsigned long long int u64 __attribute__((shared));

int
main()
{
  __atomic_store_n (&u32, 0, MEMMODEL_SEQ_CST);
  __atomic_store_n (&u64, 0, MEMMODEL_SEQ_CST);

  return 0;
}

/* { dg-final { scan-assembler-times "st.shared.b32" 1 } } */
/* { dg-final { scan-assembler-times "st.shared.b64" 1 } } */
/* { dg-final { scan-assembler-times "membar.cta" 4 } } */
