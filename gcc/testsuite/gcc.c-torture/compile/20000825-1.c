typedef signed int      s32;
typedef signed long     s64;
typedef unsigned int    u32;
typedef unsigned long   u64;

extern __inline__ u32 foobar(int logmask)
{
        u32 ret = ~(1 << logmask);      // fails
        // s32 ret = ~(1 << logmask);   // ok
        // u64 ret = ~(1 << logmask);   // ok
        // s64 ret = ~(1 << logmask);   // ok
        return ret;
}

// This procedure compiles fine...
u32 good(u32 var)
{
        var = foobar(0);
        return var;
}

// This procedure does not compile...
// Same as above, but formal parameter is a pointer
// Both good() and fails() compile ok if we choose
// a different type for "ret" in foobar().
u32 fails(u32 *var)
{
        *var = foobar(0);
        return *var;
}

