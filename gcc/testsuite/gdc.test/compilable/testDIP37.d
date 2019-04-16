// PERMUTE_ARGS:
// REQUIRED_ARGS: -Icompilable/extra-files
// EXTRA_FILES: extra-files/pkgDIP37/datetime/package.d
// EXTRA_FILES: extra-files/pkgDIP37/datetime/common.d
// EXTRA_FILES: extra-files/pkgDIP37/test17629/package.di
// EXTRA_FILES: extra-files/pkgDIP37/test17629/common.di

void test1()
{
    import pkgDIP37.datetime;
    def();
    pkgDIP37.datetime.def();
    pkgDIP37.datetime.common.def();
}

void test3()
{
    import pkgDIP37.datetime.common;
    def();
    pkgDIP37.datetime.def();
    pkgDIP37.datetime.common.def();
}

void test4()
{
    import pkgDIP37.datetime : def;
    def();
    static assert(!__traits(compiles, pkgDIP37.datetime.def()));
    static assert(!__traits(compiles, pkgDIP37.datetime.common.def()));
}

void test7()
{
    static import pkgDIP37.datetime;
    static assert(!__traits(compiles, def()));
    pkgDIP37.datetime.def();
    pkgDIP37.datetime.common.def();
}

// https://issues.dlang.org/show_bug.cgi?id=17629
void test17629()
{
    import pkgDIP37.test17629;
    foo17629();
}
