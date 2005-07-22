/* { dg-do compile { target bfin-*-* } } */
/* { dg-options "-O2 -mlong-calls" } */
/* { dg-final { scan-assembler-not "call\[^\\n\]*foo" } } */
/* { dg-final { scan-assembler-not "jump\[^\\n\]*foo" } } */
/* { dg-final { scan-assembler-not "call\[^\\n\]*baz" } } */
/* { dg-final { scan-assembler-not "jump\[^\\n\]*baz" } } */
/* { dg-final { scan-assembler "call\[^\\n\]*bar" } } */
/* { dg-final { scan-assembler "jump\[^\\n\]*bar" } } */

extern void foo () __attribute__((longcall));
extern void bar () __attribute__((shortcall));
extern void baz ();

int t1 ()
{
    foo ();
    bar ();
    baz ();
    return 4;
}

void t2 ()
{
    foo ();
}
void t3 ()
{
    bar ();
}
void t4 ()
{
    baz ();
}
