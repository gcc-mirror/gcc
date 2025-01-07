// REQUIRED_ARGS: -Jcompilable/imports/
// EXTRA_FILES: imports/imp16088.d imports/test21227/a..b.txt imports/test21227/a.txt imports/test21227/..foo/a.txt

// https://issues.dlang.org/show_bug.cgi?id=16088

void bar(string x) {}
auto foo()
{
    import("imp16088.d").bar;
}


// https://issues.dlang.org/show_bug.cgi?id=21227

void test21227()
{
    import("./test21227/a.txt").bar;
    import("test21227//a..b.txt").bar;
    import("test21227/..foo/a.txt").bar;

    version(Windows)
    {
        import(r".\test21227\a.txt").bar;
        import(r"test21227\\a..b.txt").bar;
        import(r"test21227\..foo\a.txt").bar;
    }
}

// Test that it's treated like a hex string, allowing implicit conversion to byte array

// Can't test whole contents because line endings may vary
enum expectedStart = "module imports.imp16088;";

immutable ubyte[] s0 = import("imp16088.d");

static assert(s0[0 .. expectedStart.length] == "module imports.imp16088;");

// https://issues.dlang.org/show_bug.cgi?id=24687

void foo(string path);
void foo(const(ubyte[]) data);

void bar1() { foo(import("imp16088.d")); } // matches both
void bar2() { foo(cast(const(ubyte[])) import("imp16088.d")); } // matches both!
