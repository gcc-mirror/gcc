// REQUIRED_ARGS: -preview=dip1021 -lowmem
// https://issues.dlang.org/show_bug.cgi?id=23978

// Note: this is a memory corruption bug.
// Memory returned by `GC.realloc` retains references to old memory in it,
// mostly because of the smallarray optimization for `Array(T)`.
// If this fails again, it might not be consistent, so try running it multiple times.

class LUBench { }
void lup(ulong , ulong , int , int = 1)
{
    new LUBench;
}
void lup_3200(ulong iters, ulong flops)
{
    lup(iters, flops, 3200);
}
void raytrace()
{
    struct V
    {
        float x, y, z;
        auto normalize() { }
        struct Tid { }
        auto spawnLinked() { }
        string[] namesByTid;
        class MessageBox { }
        auto cross() { }
    }
}
