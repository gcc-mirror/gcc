module imports.a13131parameters;

auto createParameterMixins()    // auto is necessary to invoke semantic3 to calculate full signature
{
    pragma(msg, "+B");
    enum fullModuleName = "imports.a13131elec";  // necessary
    mixin("import "~fullModuleName~";");
    foreach (e ; __traits(derivedMembers, mixin(fullModuleName)))
    {
        // will access yet-not semantic analyzed invalid symbol 'econn' in imports.elec
        static if ( __traits(compiles, mixin(`__traits(getAttributes, `~fullModuleName~`.`~e~`)`))) {}
    }
    pragma(msg, "-B");
}
