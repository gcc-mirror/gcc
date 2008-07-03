/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic" } */

#define f(a,b)   f2(a,,b) 
#define f2(a,b,c) a; b; c;
#define f3(a)    a

#define g()   p()

void p(void) {}


void foo(void)
{
    f(p(),p()); /* { dg-warning "macro f2 argument 2: empty macro arguments are undefined" } */
    f2(p(),,p()); /* { dg-warning "macro f2 argument 2: empty macro arguments are undefined" } */
    f3(); /* { dg-warning "macro f3 argument 1: empty macro arguments are undefined" } */
    g();
}
