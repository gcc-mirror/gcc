// REQUIRED_ARGS: -de
// PERMUTE_ARGS:

class LeClass {
    import std.stdio;
}

void main()
{
    static assert([__traits(allMembers, LeClass)] == ["toString", "toHash", "opCmp", "opEquals", "Monitor", "factory"]);
}

