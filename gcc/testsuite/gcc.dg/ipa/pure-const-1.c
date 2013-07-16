/* { dg-do compile { target nonpic } } */
/* { dg-options "-O3 -fdump-tree-local-pure-const1 -fdump-ipa-pure-const -fdump-tree-optimized -fno-early-inlining" } */
void abort (void);
int error_code;
static int val;
__attribute__ ((noinline, noclone))
static int
i_am_pure1 (int a)
{
  if (a > 50)
    abort ();
  return a;
}

__attribute__ ((noinline, noclone))
static int
i_am_const2 (int a)
{
  return a+val;
}

__attribute__ ((noinline, noclone))
int
call_me(int a)
{
  return a;
}

inline int
call_callback(int (*fn)(int), int a)
{
  return fn(a);
}

__attribute__ ((noinline, noclone))
i_am_const3(int a)
{
  return call_callback (call_me, a);
}

__attribute__ ((noinline))
explode_badly()
{
  error_code = 0xbad;
  abort ();
}

__attribute__ ((noinline, noclone))
i_am_pure4(int a)
{
  if (a > 50)
    explode_badly ();
  return a;
}

test()
{
  int s;
  s = i_am_pure1(5);
  s += i_am_pure1(5);
  s += i_am_const2(5);
  s += i_am_const2(5);
  s += i_am_const3(5);
  s += i_am_const3(5);
  s += i_am_pure4(5);
  s += i_am_pure4(5);
  return s;
}
/* { dg-final { scan-tree-dump-times "i_am_pure1 .5" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "i_am_const2 .5" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "i_am_const3 .5" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "i_am_pure4 .5" 1 "optimized"} } */
/* { dg-final { scan-tree-dump "found to be looping pure: i_am_pure1" "local-pure-const1"} } */
/* { dg-final { scan-tree-dump "found to be looping pure: i_am_pure4" "local-pure-const1"} } */
/* { dg-final { scan-ipa-dump "found to be const: i_am_const2" "pure-const"} } */
/* { dg-final { scan-ipa-dump "found to be const: i_am_const3" "pure-const"} } */
/* { dg-final { cleanup-tree-dump "local-pure-const1" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
/* { dg-final { cleanup-ipa-dump "pure-const" } } */

