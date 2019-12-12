/* { dg-do compile } */
/* { dg-options "-std=c99" } */

int a0;

struct S
{
    int a1;
    union {
        int a0;
        int a1; /* { dg-error "duplicate member" } */
        int a2, a3, a4, a5, a6, a7, a8, a9;
        int a10, a11, a12, a13, a14, a15;
    };
};

int f()
{
    struct S s;
    return s.a0;
}
