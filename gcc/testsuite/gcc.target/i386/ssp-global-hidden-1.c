/* { dg-do run { target { fstack_protector && fpic } } } */
/* { dg-options "-O2 -fPIC -fstack-protector-all -mstack-protector-guard=global -save-temps" } */

#include <stdint.h>
#include <stdlib.h>

__attribute__ ((visibility ("hidden")))
#ifdef __LP64__
uintptr_t __stack_chk_guard = 0x2d853605a4d9a09cUL;
#else
uintptr_t __stack_chk_guard = 0xdd2cc927UL;
#endif

void
__stack_chk_fail (void)
{
  exit (0); /* pass */
}

__attribute__ ((noipa))
void
smash (char *p, int i)
{
  p[i] = 42;
}

int
main (void)
{
  char foo[255];

   /* smash stack */
  for (int i = 0; i <= 400; i++)
    smash (foo, i);

  return 1;
}

/* { dg-final { scan-hidden "__stack_chk_guard" } } */
/* { dg-final { scan-assembler "__stack_chk_guard\\(%rip\\)" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "__stack_chk_guard@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler ".quad	3280087301477736604" { target { lp64 } } } } */
/* { dg-final { scan-assembler "__stack_chk_guard@GOTOFF" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler-not "__stack_chk_guard@GOT\\(" { target ia32 } } } */
/* { dg-final { scan-assembler ".long	-584267481" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler {___stack_chk_guard-L.\$pb} { target { ia32 && *-*-darwin* } } } } */
