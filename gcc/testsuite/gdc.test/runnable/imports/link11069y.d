module imports.link11069y;
import std.traits;

void readWriteVariable(T)(ref T data)
{
    foreach (it; __traits(allMembers, T))
    {
        enum vValid = mixin(`is(FunctionTypeOf!(T.` ~ it ~ `) == function)`);
    }
}
