/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-options "-Wall" } */
/* Testcase to check for ABI compliance of parameter passing
   for the PowerPC64 ABI.  */

typedef int __attribute__((vector_size(16))) v4si;
typedef int __attribute__((vector_size(8))) v2si;

v4si 
f(v4si v)
{ /* { dg-error "altivec instructions are disabled" "PR18631" { xfail *-*-* } } */
    return v;
}

v2si 
g(v2si v)
{
    return v;
}

int 
main()
{
    v4si v = { 1, 2, 3, 4 };
    v2si w = { 5, 6 };
    v = f (v); /* { dg-error "altivec instructions are disabled" "PR18631" { xfail *-*-* } } */
    w = g (w);
    return 0;
}
