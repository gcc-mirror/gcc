// REQUIRED_ARGS: -Jcompilable/imports
// EXTRA_FILES: imports/test21227/a..b.txt imports/test21227/a.txt imports/test21227/..foo/a.txt

// https://issues.dlang.org/show_bug.cgi?id=21227

void bar(string x) {}
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
