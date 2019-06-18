// REQUIRED_ARGS: -unittest
// Issue 21285 - Delegate covariance broken between 2.092 and 2.094 (git master). 
unittest
{
    string path;
    int bank;
    static string path2;
    static int bank2;
    
    // delegates
    auto a = [
        (string arg) { path = arg; },
        (string arg) { bank = 1; throw new Exception(""); }
    ];
    
    // functions
    auto ab = [
        (string arg) { path2 = arg; },
        (string arg) { bank2 = 1; throw new Exception(""); }
    ];
    
    alias dg = void delegate(string) pure @safe;
    alias fn = void function(string) @safe;
    
    static assert(is(typeof(a[0]) == dg));
    static assert(is(typeof(ab[0]) == fn));
}
