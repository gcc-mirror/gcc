/* { dg-do run { target x32 } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=long" } */

extern void abort (void);

long long li;

long long 
__attribute__ ((noinline))
testfunc (void* addr)
{
  li = (long long)(int)addr;
  li &= 0xffffffff;
  return li;
}

int main (void)
{
  volatile long long rv_test;
  rv_test = testfunc((void*)0x87651234);
  if (rv_test != 0x87651234ULL)
    abort ();

  return 0;
}
