// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import gcc.simd;

void main ()
{
    static if (__traits(compiles, __vector(int[4])))
        alias int4 = __vector(int[4]);
    static if (__traits(compiles, __vector(uint[4])))
        alias uint4 = __vector(uint[4]);
    static if (__traits(compiles, __vector(float[4])))
        alias float4 = __vector(float[4]);
    static if (__traits(compiles, __vector(double[4])))
        alias double4 = __vector(double[4]);

    static if (__traits(compiles, int4))
    {
        union U1 { int4 v; int[4] a; }
        U1 u1;
    }
    static if (__traits(compiles, uint4))
    {
        union U2 { uint4 v; uint[4] a; }
        U2 u2;
    }
    static if (__traits(compiles, float4))
    {
        union U3 { float4 v; float[4] a; }
        U3 u3;
    }
    static if (__traits(compiles, double4))
    {
        union U4 { double4 v; double[4] a; }
        U4 u4;
    }

    static if (__traits(compiles, u1) && __traits(compiles, u2))
    {
        static void f1(ref uint4 x, out int4 y)
        {
            y = convertvector!int4(x);
        }
        static foreach (i; 0 .. 4)
            u2.a[i] = i * 2;
        f1(u2.v, u1.v);
        static foreach (i; 0 .. 4)
            assert(u1.a[i] == i * 2);
    }

    static if (__traits(compiles, u1) && __traits(compiles, u3))
    {
        static void f2(ref float4 x, out int4 y)
        {
            y = convertvector!int4(x);
        }

        static void f3(ref int4 x, out float4 y)
        {
            y = convertvector!float4(x);
        }

        static foreach (i; 0 .. 4)
            u3.a[i] = i - 2.25f;
        f2(u3.v, u1.v);
        static foreach (i; 0 .. 4)
            assert(u1.a[i] == (i == 3 ? 0 : i - 2));

        static foreach (i; 0 .. 4)
            u3.a[i] = i + 0.75f;
        f2(u3.v, u1.v);
        static foreach (i; 0 .. 4)
            assert(u1.a[i] == i);

        static foreach (i; 0 .. 4)
            u1.a[i] = 7 * i - 5;
        f3(u1.v, u3.v);
        static foreach (i; 0 .. 4)
            assert(u3.a[i] == 7 * i - 5);
    }
    static if (__traits(compiles, u1) && __traits(compiles, u4))
    {
        static void f4(ref double4 x, out int4 y)
        {
            y = convertvector!int4(x);
        }

        static void f5(ref int4 x, out double4 y)
        {
            y = convertvector!double4(x);
        }

        static foreach (i; 0 .. 4)
            u4.a[i] = i - 2.25;
        f4(u4.v, u1.v);
        static foreach (i; 0 .. 4)
            assert(u1.a[i] == (i == 3 ? 0 : i - 2));

        static foreach (i; 0 .. 4)
            u4.a[i] = i + 0.75;
        f4(u4.v, u1.v);
        static foreach (i; 0 .. 4)
            assert(u1.a[i] == i);

        static foreach (i; 0 .. 4)
            u1.a[i] = 7 * i - 5;
        f5(u1.v, u4.v);
        static foreach (i; 0 .. 4)
            assert(u4.a[i] == 7 * i - 5);
    }
    static if (__traits(compiles, u4))
    {
        static void f6(out double4 x)
        {
            int4 a = [1, 2, -3, -4];
            x = convertvector!double4(a);
        }

        f6(u4.v);
        static foreach (i; 0 .. 4)
            assert(u4.a[i] == (i >= 2 ? -1 - i : i + 1));
    }
}
