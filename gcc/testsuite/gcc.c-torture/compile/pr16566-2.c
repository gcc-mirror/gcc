/* ICE with flexible arrays in non-lvalue structures.  Bug 16566
   (comment #5).  */
/* { dg-options "-Wno-psabi" { target { { i?86-*-* x86_64-*-* } && lp64 } } } */

struct A
{
    int i;
    int x[];
};

int foo(struct A a)
{ 
    return (a,a).x[0];
}
