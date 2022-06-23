// { dg-do compile }
// { dg-options "-Wall -O2 -fdump-tree-optimized" }

import gcc.attributes;

pragma(inline, true)
void syscall()(int val)
{
    @register("4") int reg = val;
    asm { "/* Some Code %0 */" :: "r" (reg); }
}

void do_syscalls()
{
    for (int s = 0; s < 2; s++)
    {
        syscall (0);
        syscall (1);
    }
}

// { dg-final { scan-tree-dump-times "reg = " 4 "optimized" } }
