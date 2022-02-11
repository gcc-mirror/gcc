// https://issues.dlang.org/show_bug.cgi?id=21464
// EXTRA_FILES: imports/test21464a.d
void foo() pure
{
    import imports.test21464a : Mallocator;
    auto a = Mallocator.instance; // mutable static, but empty struct
}
