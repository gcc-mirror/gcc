/* { dg-do compile } */
/* { dg-options "-w -O2 -fdump-tree-optimized" } */

static __inline__ __attribute__((__always_inline__))
void syscall_7 (int val)
{
  register int reg __asm ("4") = val;
  __asm __volatile__ ("/* Some Code %0 */" :: "r" (reg));
}

void do_syscalls (void)
{
  for (int s = 0; s < 2; s++)
    {
      syscall_7 (0);
      syscall_7 (1);
    }
}

/* { dg-final { scan-tree-dump-times "reg = " 4 "optimized" } } */
