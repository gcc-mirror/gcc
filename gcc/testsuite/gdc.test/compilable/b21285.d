// REQUIRED_ARGS: -unittest
// PERMUTE_ARGS: -preview=dip1000
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

int f(string s) { throw new Exception(""); }
void main()
{
    string path;
    int bank, preset;
    void delegate(string value)[string] aa = [
        "path": (string arg) {
            path = arg;
        },
        "bank": (string arg) {
            bank = f(arg);
        },
        "preset": (string arg) {
            preset = f(arg);
        },
    ];

    string delegate(string value)[string] aa2 = [
        "path": (string arg) {
            path = arg;
            return arg;
        },
        "bank": (string arg) {
            bank = f(arg);
            return arg;
        },
        "preset": (string arg) {
            preset = f(arg);
            return arg;
        },
    ];
}
