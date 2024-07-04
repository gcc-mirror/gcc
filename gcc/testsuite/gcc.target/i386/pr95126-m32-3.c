/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -mno-accumulate-outgoing-args" } */

struct small{ short a; };

void call_func(void)
{
    extern int func(struct small X);
    static struct small const s = { 2 };
    func(s);
}

/* { dg-final { scan-assembler "pushl\[ \\t]*\\\$2" } } */
/* { dg-final { scan-assembler-not "movzwl" } } */

