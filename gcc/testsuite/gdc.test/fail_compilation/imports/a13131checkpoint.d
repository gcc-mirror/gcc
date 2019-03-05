module imports.a13131checkpoint;

mixin(createGlobalsMixins);             // [3] mixin

auto createGlobalsMixins()              // [4] semantic3
{
    pragma(msg, "+A");
    enum fullModuleName = "imports.a13131parameters";  // necessary
    mixin("import "~fullModuleName~";");
    foreach (e ; __traits(derivedMembers, mixin(fullModuleName)))
    {
        // [5] see imports.parameters (it's listed in command line)
        static if ( __traits(compiles, mixin(`__traits(getAttributes, `~fullModuleName~`.`~e~`)`))) {}
    }
    pragma(msg, "-A");

    return "";
}
