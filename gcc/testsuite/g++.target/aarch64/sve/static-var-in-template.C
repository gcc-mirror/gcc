/* { dg-do compile } */

#include <arm_sve.h>

template <int N>
void f()
{
    static svbool_t pg = svwhilelt_b64(0, N);
}

int main(int argc, char **argv)
{
    f<2>();
    return 0;
}

/* { dg-error "SVE type 'svbool_t' does not have a fixed size" "" { target *-*-* } 0 } */
