// COMPILE_SEPARATELY:
// EXTRA_SOURCES: imports/test7494a.d
// PERMUTE_ARGS:
// REQUIRED_ARGS:

module test7494;

void main()
{
    import imports.test7494a : map;             // selective
    import imports.test7494a : put = writeln;   // selective + rename
    auto r = map!(a=>a)([1,2,3]);
    assert(r == [4,5,6]);
    put(r);
    static assert(!__traits(compiles, foo()));

    import core.bitop : bsr;
    // ^ or just any selective import statements
    bsr(1);
}
