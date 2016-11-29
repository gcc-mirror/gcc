/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fdump-tree-dom-details" } */


static inline long load(long *p)
{
        long ret;
        asm ("xyzzy      %1,%0\n\t" : "=r" (ret) : "m" (*p));
        if (ret != *p)
                __builtin_unreachable();
        return ret;
}

long foo(long *mem)
{
        long ret;
        ret = load(mem);
        return ret - *mem;
}

/* { dg-final { scan-tree-dump "Folded to: _\[0-9\]+ = 0;"  "dom2" } } */
