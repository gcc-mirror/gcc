/* { dg-options "-fgnu-tm" } */

extern int i;

main()
{
  __transaction_atomic { i = 0; }
}

#define dummy(func)							\
  __attribute__((noinline,noclone,used)) void func() { asm (""); }

dummy(_ITM_beginTransaction)
dummy(_ITM_commitTransaction)
dummy(_ITM_WU4)
dummy(_ITM_WU8)
dummy(_ITM_registerTMCloneTable)
dummy(_ITM_deregisterTMCloneTable)
