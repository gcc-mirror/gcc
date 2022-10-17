/* PR target/101716 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler "leal\[\\t \]\[^\\n\]*eax" } } */
/* { dg-final { scan-assembler-not "movl\[\\t \]\[^\\n\]*eax" } } */

unsigned long long sample1(unsigned long long m) {
    unsigned int t = -1;
    return (m << 1) & t;
}
