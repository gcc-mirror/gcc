/* Test that the ms_hook_prologue attribute generates the correct code.  */

/* { dg-do run } */
/* { dg-require-effective-target ms_hook_prologue } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

int __attribute__ ((__ms_hook_prologue__)) foo ()
{
  unsigned char *ptr = (unsigned char *) foo;

  /* The NOP mov must not be optimized away by optimizations.
     The push %ebp, mov %esp, %ebp must not be removed by
     -fomit-frame-pointer */
#ifndef __x86_64__
  /* movl.s %edi, %edi */
  if(*ptr++ != 0x8b) return 1;
  if(*ptr++ != 0xff) return 1;
  /* push %ebp */
  if(*ptr++ != 0x55) return 1;
  /* movl.s %esp, %ebp */
  if(*ptr++ != 0x8b) return 1;
  if(*ptr++ != 0xec) return 1;
#else
  /* leaq 0(%rsp), %rsp */
  if (*ptr++ != 0x48) return 1;
  if (*ptr++ != 0x8d) return 1;
  if (*ptr++ != 0xa4) return 1;
  if (*ptr++ != 0x24) return 1;
  if (ptr[0] != 0 || ptr[1] != 0 || ptr[2] != 0 || ptr[3] != 0)
    return 1;
#endif
  return 0;
}

int main ()
{
  return foo();
}
