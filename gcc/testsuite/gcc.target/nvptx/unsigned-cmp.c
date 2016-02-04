/* { dg-options "-O2" } */
/* { dg-do run } */

/* nvptx backend used to emit lo/ls/hs/hi suffixes on unsigned comparison
   insns instead of the more common lt/le/ge/gt, but ptxas and PTX JIT
   miscompile 'ls' and 'hi' under some circumstances, such as when the first
   source operand expands to a constant memory load, as demonstrated below.
   Reported as NVIDIA bug ID 1725195 (tracker is not public).  */

/* Define this to observe PTX translation breakage.  */
//#define EMIT_BROKEN_ASM 1

/* Or define this to get expected codegen.  */
//#define EMIT_WORKING_ASM 1

static __attribute__((noinline,noclone)) int ls(unsigned a)
{
    unsigned v;
    /* %nctaid.x is always 1 in gcc testing.  */
    asm ("mov.u32 %0, %%nctaid.x;" : "=r"(v));
#if defined(EMIT_BROKEN_ASM)
    asm ("set.u32.ls.u32 %0, %1, %0;" : "+r"(a) : "r"(v));
#elif defined(EMIT_WORKING_ASM)
    asm ("set.u32.le.u32 %0, %1, %0;" : "+r"(a) : "r"(v));
#else
    a = v <= a ? -1 : 0;
#endif
    return a;
}
static __attribute__((noinline,noclone)) int hi(unsigned a)
{
    unsigned v;
    asm ("mov.u32 %0, %%nctaid.x;" : "=r"(v));
#if defined(EMIT_BROKEN_ASM)
    asm ("set.u32.hi.u32 %0, %1, %0;" : "+r"(a) : "r"(v));
#elif defined(EMIT_WORKING_ASM)
    asm ("set.u32.gt.u32 %0, %1, %0;" : "+r"(a) : "r"(v));
#else
    a = v > a ? -1 : 0;
#endif
    return a;
}
int main()
{
    int i;
    for (i=0; i<3; i++)
        if (ls(i) != -(1 <= i) || hi(i) != -(1 > i))
            __builtin_abort();
    return 0;
}
