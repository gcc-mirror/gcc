/* { dg-do compile { target lp64 } } */
/* { dg-options "-maccumulate-outgoing-args" } */

int __attribute__((__noinline__))
bugged(float f1, float f2, float f3, float f4,
       float f5, float f6, float f7, float f8)
{
    return f1 || f2 || f3 || f4 || f5 != 1. || f6 != 1. || f7 != 1. || f8 != 1.;
}

int __attribute__((__noinline__, __ms_abi__)) isbugged(void)
{
    return bugged(0, 0, 0, 0, 1., 1., 1., 1.);
}

int main()
{
    return isbugged();
}

