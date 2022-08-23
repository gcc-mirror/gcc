// EXTRA_CPP_SOURCES: test6716.cpp

version(Windows)
{
    // without main, there is no implicit reference to the runtime library
    // other platforms pass the runtime library on the linker command line
    version(CRuntime_Microsoft)
        version(Win64)
            pragma(lib, "phobos64");
        else
            pragma(lib, "phobos32mscoff");
    else
        pragma(lib, "phobos");
}

extern(C++) int test6716(int magic)
{
    assert(magic == 12345);
    return 0;
}
