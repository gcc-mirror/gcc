/* { dg-lto-options {{-flto -fgnu-tm}} } */

extern void foobar() __attribute__((transaction_callable));

#define dummy(func) \
  __attribute__((noinline,noclone,used)) void func() { asm (""); }

dummy(_ITM_beginTransaction)
dummy(_ITM_commitTransaction)
dummy(_ITM_WU4)
dummy(_ITM_WU8)

main()
{
  __transaction_relaxed
    {
      foobar();
    }
}
