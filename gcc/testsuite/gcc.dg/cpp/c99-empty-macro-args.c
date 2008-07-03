/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic" } */

#define f(a,b)   f2(a,,b) 
#define f2(a,b,c) a; b; c;
#define f3(a)    a

#define g()   p()

void p(void) {}


void foo(void)
{
    f(p(),p());
    f2(p(),,p());
    f3();
    g();
}
