/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2" } */

struct small{ short a,b; signed char c; };

void call_func(void)
{
    extern int func(struct small X);
    static struct small const s = { 1,2,0 };
    func(s);
}

/* The @GOTOFF addressing seems to prevent the optimization of the loads to
   known constants.  */
/* { dg-final { scan-assembler "movl\[ \\t]*\\\$" { xfail { ! nonpic } } } } */
/* { dg-final { scan-assembler "movb\[ \\t]*\\\$0, " { xfail { ! nonpic } } } } */
/* { dg-final { scan-assembler-not "movzwl" { xfail { ! nonpic } } } } */
