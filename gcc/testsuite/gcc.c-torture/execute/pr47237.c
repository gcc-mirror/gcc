/* { dg-xfail-run-if "can cause stack underflow" { nios2-*-* amdgcn-*-* } } */
/* { dg-require-effective-target untyped_assembly } */
#define INTEGER_ARG  5

extern void abort(void);

static void foo(int arg)
{
  if (arg != INTEGER_ARG)
    abort();
}

static void bar(int arg)
{
  foo(arg);
  __builtin_apply(foo, __builtin_apply_args(), 16);
}

int main(void)
{
  bar(INTEGER_ARG);

  return 0;
}
