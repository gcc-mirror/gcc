/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

struct small{ short a,b; };

void call_func(void)
{
    extern int func(struct small X);
    static struct small const s = { 1,2 };
    func(s);
}

/* { dg-final { scan-assembler "movl\[ \\t]*\\\$131073, " } } */
