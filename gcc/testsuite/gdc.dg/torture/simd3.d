// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

struct Sunsto
{
  align (1): // make sure f4 is misaligned
    byte b;
    union
    {
        float4 f4;
        ubyte[16] a;
    }
}

ubyte[16] foounsto()
{
    float4 vf = 6;
    Sunsto s;
    s.f4 = vf * 2;
    vf = s.f4;

    return s.a;
}

void main()
{
    auto a = foounsto();
    version (LittleEndian)
        assert(a == [0, 0, 64, 65, 0, 0, 64, 65, 0, 0, 64, 65, 0, 0, 64, 65]);
    version (BigEndian)
        assert(a == [65, 64, 0, 0, 65, 64, 0, 0, 65, 64, 0, 0, 65, 64, 0, 0]);
}
