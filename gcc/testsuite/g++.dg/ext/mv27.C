// PR c++/83911
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-require-ifunc "" }

class SimdFloat
{
public:
    __attribute__ ((target ("default")))
    SimdFloat(float x) {}

    __attribute__ ((target ("avx2")))
    SimdFloat(float x) {}
};

SimdFloat foo()
{
    return 1;
}
