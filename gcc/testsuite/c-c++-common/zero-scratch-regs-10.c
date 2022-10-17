/* { dg-do run } */
/* { dg-skip-if "not implemented" { ! { i?86*-*-* x86_64*-*-* sparc*-*-* aarch64*-*-* nvptx*-*-* s390*-*-* loongarch64*-*-* } } } */
/* { dg-options "-O2" } */

#include <assert.h>
int result = 0;

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("skip")))
foo1 (int x)
{
  return (x + 1);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("used-gpr-arg")))
foo2 (int x)
{
  return (x + 2);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("used-gpr")))
foo3 (int x)
{
  return (x + 3);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("used-arg")))
foo4 (int x)
{
  return (x + 4);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("used")))
foo5 (int x)
{
  return (x + 5);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("all-gpr-arg")))
foo6 (int x)
{
  return (x + 6);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("all-gpr")))
foo7 (int x)
{
  return (x + 7);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("all-arg")))
foo8 (int x)
{
  return (x + 8);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("all")))
foo9 (int x)
{
  return (x + 9);
}

int main()
{
  result = foo1 (1);
  result += foo2 (1);
  result += foo3 (1);
  result += foo4 (1);
  result += foo5 (1);
  result += foo6 (1);
  result += foo7 (1);
  result += foo8 (1);
  result += foo9 (1);
  assert (result == 54);
  return 0;
}
