/* PR target/40838 */
/* { dg-do compile { target { { ! *-*-darwin* } && ilp32 } } } */
/* { dg-options "-w -mstackrealign -O2 -msse2 -mpreferred-stack-boundary=4" } */
/* { dg-require-effective-target sse2 } */

typedef int v4si __attribute__ ((vector_size (16)));

struct x {
       v4si v;
       v4si w;
};

void y(void *);

v4si x(void)
{
       struct x x;
       y(&x);
}

/* { dg-final { scan-assembler "andl\[\\t \]*\\$-16,\[\\t \]*%esp" } } */
