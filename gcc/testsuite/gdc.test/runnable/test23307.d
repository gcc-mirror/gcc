// REQUIRED_ARGS: -inline -O

// https://issues.dlang.org/show_bug.cgi?id=23307
import core.simd;

static if(__traits(compiles, int4))
{

alias __m128i = int4;

uint bitwiseRotateRight_uint(const uint value, const uint count)
{
    assert(count < 8 * uint.sizeof);
    return cast(uint) ((value >> count) | (value << (uint.sizeof * 8 - count)));
}


__m128i _mm_sha256rnds2_epu32(__m128i a, __m128i b, __m128i k)
{

    static uint Ch(uint x, uint y, uint z)
    {
        return z ^ (x & (y ^ z));
    }

    static uint Maj(uint x, uint y, uint z)
    {
        return (x & y) | (z & (x ^ y));
    }

    static uint sum0(uint x)
    {
        return bitwiseRotateRight_uint(x, 2) ^ bitwiseRotateRight_uint(x, 13) ^ bitwiseRotateRight_uint(x, 22);
    }

    static uint sum1(uint x)
    {
        return bitwiseRotateRight_uint(x, 6) ^ bitwiseRotateRight_uint(x, 11) ^ bitwiseRotateRight_uint(x, 25);
    }

    int4 dst;
    int4 a4 = cast(int4) a;
    int4 b4 = cast(int4) b;
    int4 k4 = cast(int4) k;

    const A0 = b4.array[3];
    const B0 = b4.array[2];
    const C0 = a4.array[3];
    const D0 = a4.array[2];
    const E0 = b4.array[1];
    const F0 = b4.array[0];
    const G0 = a4.array[1];
    const H0 = a4.array[0];
    const W_K0 = k4.array[0];
    const W_K1 = k4.array[1];
    const A1 = Ch(E0, F0, G0) + sum1(E0) + W_K0 + H0 + Maj(A0, B0, C0) + sum0(A0);
    const B1 = A0;
    const C1 = B0;
    const D1 = C0;
    const E1 = Ch(E0, F0, G0) + sum1(E0) + W_K0 + H0 + D0;
    const F1 = E0;
    const G1 = F0;
    const H1 = G0;
    const A2 = Ch(E1, F1, G1) + sum1(E1) + W_K1 + H1 + Maj(A1, B1, C1) + sum0(A1);
    const B2 = A1;
    const C2 = B1;
    const D2 = C1;
    const E2 = Ch(E1, F1, G1) + sum1(E1) + W_K1 + H1 + D1;
    const F2 = E1;
    const G2 = F1;
    const H2 = G1;

    dst.ptr[3] = A2;
    dst.ptr[2] = B2;
    dst.ptr[1] = E2;
    dst.ptr[0] = F2;

    return cast(__m128i) dst;
}

void main(string[] args)
{
    __m128i a = [15, 20, 130, 12345];
    __m128i b = [15, 20, 130, 12345];
    __m128i k = [15, 20, 130, 12345];
    __m128i result = _mm_sha256rnds2_epu32(a, b, k);
    assert(result.array == [1384123044, -2050674062, 327754346, 956342016]);
}
}
else
{
int main() { return 0; }
}
