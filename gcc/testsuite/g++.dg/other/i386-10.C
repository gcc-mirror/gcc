// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options -maes }

typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));

int main()
{
    const char index = 1;
    __m128i r = { };

    r = __builtin_ia32_aeskeygenassist128 (r, (int)(index));
}
