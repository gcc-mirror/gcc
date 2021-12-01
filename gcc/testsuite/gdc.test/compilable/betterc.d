/* REQUIRED_ARGS: -betterC
*/

// https://issues.dlang.org/show_bug.cgi?id=17787
version (D_BetterC)
{
}
else
{
    static assert(0);
}

// -betterC does not support `ModuleInfo`, `TypeInfo`, or exception handling
version (D_ModuleInfo)
{
    static assert(0);
}

version (D_Exceptions)
{
    static assert(0);
}

version (D_TypeInfo)
{
    static assert(0);
}
