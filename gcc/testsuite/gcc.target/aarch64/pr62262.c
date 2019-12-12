/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-use -Wno-missing-profile" } */

static inline int CLZ(int mask) {
   return mask ? __builtin_clz(mask) : 32;
}

int foo(int value)
{
    if (value == 0)
        return 0;

    int bias = CLZ(value);
    value >>= bias;
    int zeros = CLZ(value << 1);
    value <<= zeros;

    int packed = (unsigned)(value << 9) >> 9;
    return packed;
}
