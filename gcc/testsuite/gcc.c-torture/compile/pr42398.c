int ptrace_setregs(void)
{
  union { unsigned int l; int t; } __gu_tmp;
  __asm__ __volatile__("" : "=r" (__gu_tmp.l));
  return __gu_tmp.t;
}
