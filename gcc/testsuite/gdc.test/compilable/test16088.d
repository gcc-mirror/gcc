// REQUIRED_ARGS: -Jcompilable/imports/
// EXTRA_FILES: imports/imp16088.d

// https://issues.dlang.org/show_bug.cgi?id=16088

void bar(string x) {}
auto foo()
{
    import("imp16088.d").bar;
}
