// REQUIRED_ARGS: -de
// PERMUTE_ARGS:

class LeClass {
    import core.stdc.stdio;
}

void main()
{
    static assert([__traits(allMembers, LeClass)] == ["toString", "toHash", "opCmp", "opEquals", "Monitor", "factory"]);
}
