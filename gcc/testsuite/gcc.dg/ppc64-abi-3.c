/* { dg-do compile { target powerpc64-*-linux* } } */
/* { dg-options "-Wall" } */
/* Testcase to check for ABI compliance of parameter passing
   for the PowerPC64 ABI.  */

typedef int __attribute__((mode(V4SI))) v4si;
typedef int __attribute__((mode(V2SI))) v2si;

v4si 
f(v4si v)
{ /* { dg-error "altivec instructions are disabled" } */
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
    v4si v;
    v2si w;
    v = f (v); /* { dg-error "altivec instructions are disabled" } */
    w = g (w);
}
