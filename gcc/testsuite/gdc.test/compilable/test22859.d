// https://issues.dlang.org/show_bug.cgi?id=22859
private struct __InoutWorkaroundStruct {}
@property T rvalueOf(T)(T val) { return val; }
@property T rvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);
@property ref T lvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);

// taken from std.traits.isAssignable
template isAssignable(Lhs, Rhs = Lhs)
{
    enum isAssignable = __traits(compiles, lvalueOf!Lhs = rvalueOf!Rhs) && __traits(compiles, lvalueOf!Lhs = lvalueOf!Rhs);
}

// taken from std.meta.allSatisfy
template allSatisfy(alias F, T...)
{
    static foreach (Ti; T)
    {
        static if (!is(typeof(allSatisfy) == bool) && // not yet defined
                   !F!(Ti))
        {
            enum allSatisfy = false;
        }
    }
    static if (!is(typeof(allSatisfy) == bool)) // if not yet defined
    {
        enum allSatisfy = true;
    }
}

struct None{}

class C1
{
    static if(allSatisfy!(isAssignable, None, C2)) {}
}

class C2
{
    static if(allSatisfy!(isAssignable, None, C1, C2)) {}
}
