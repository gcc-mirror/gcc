/* { dg-options "-O2" } */

#include <arm_sve.h>
typedef int __attribute__((vector_size(8))) v2si;
typedef struct { int x; int y; } A;
void bar(A a);
void foo()
{
    A a;
    *(v2si *)&a = (v2si){0, (int)svcntd_pat(SV_ALL)};
    bar(a);
}
