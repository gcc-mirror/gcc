/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

static __attribute__((always_inline)) inline
int __atomic_dec_and_test(int *ptr)
{
        int cc;
        asm volatile(
                "       alsi    %[ptr],-1\n"
                : "=@cc" (cc), [ptr] "+QS" (*ptr) : : "memory");
        return (cc == 0) || (cc == 2);
}
 
int a;
void dummy(void);
long fu(void)
{
        if (__atomic_dec_and_test(&a))
                return 5;
        return 8;
}
 
void bar(void)
{
        if (__atomic_dec_and_test(&a))
                dummy();
}

int foo(int x)
{
        int cc;
        asm volatile ("ahi %[x],42\n"
                        : [x] "+d" (x), "=@cc" (cc));
        return !(cc == 0 || cc == 2) ? 42 : 13;
}

/* { dg-final { scan-assembler-not {ipm} } } */
