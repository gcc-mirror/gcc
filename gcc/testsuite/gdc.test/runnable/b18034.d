// REQUIRED_ARGS: -O
import core.simd;

static if (__traits(compiles, { void16 a; ushort8 b; }))
{
    void check(void16 a)
    {
        foreach (x; (cast(ushort8)a).array)
        {
	        assert(x == 1);
        }
    }

    void make(ushort x)
    {
        ushort8 v = ushort8(x);
        check(v);
    }

    void main()
    {
        make(1);
    }
}
else
{
    void main() { }
}
