/* { dg-lto-do run } */
/* { dg-lto-options { { -O3 -flto } } } */
/* This testcase tests that anonymous namespaces in different TUs are treated
   as different types by LTO TBAA and that they never alias with structurally
   same C types.  */
namespace {
  __attribute__((used))
  struct a {int a;} *p,**ptr=&p;
};
void
set1()
{
  *ptr=0;
}
void
get1()
{
  if (!__builtin_constant_p (*ptr==0))
    __builtin_abort ();
}
extern void set2();
extern "C" void set3();
int n = 1;
int
main()
{
  for (int i = 0; i < n; i++)
    {
      set1();
      set2();
      set3();
      get1();
    }
  return 0;
}
