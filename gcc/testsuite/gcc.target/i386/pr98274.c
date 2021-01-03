/* PR target/98274 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-mabi=sysv -O2 -march=x86-64-v2" } */

void __attribute__((target ("avx")))
foo (void)
{
}
