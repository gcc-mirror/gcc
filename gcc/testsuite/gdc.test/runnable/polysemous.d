// PERMUTE_ARGS:

// Prefer immutable(char)[] to all others
int foo(           char[] a) { return 11; }
int foo(    const  char[] a) { return 12; }
int foo(immutable  char[] a) { return 13; }
int foo(          wchar[] a) { return 21; }
int foo(    const wchar[] a) { return 22; }
int foo(immutable wchar[] a) { return 23; }
int foo(          dchar[] a) { return 31; }
int foo(    const dchar[] a) { return 32; }
int foo(immutable dchar[] a) { return 33; }

// Prefer const conversion over polysemous conversion
int bar(           char[] a) { return 11; }
int bar(    const  char[] a) { return 12; }
//  bar(immutable  char[] a);
int bar(          wchar[] a) { return 21; }
int bar(    const wchar[] a) { return 22; }
//  bar(immutable wchar[] a);
int bar(          dchar[] a) { return 31; }
int bar(    const dchar[] a) { return 32; }
//  bar(immutable dchar[] a);

// No conversion to mutable
int baz(           char[] a) { return 11; }
//  baz(    const  char[] a);
//  baz(immutable  char[] a);
int baz(          wchar[] a) { return 21; }
//  baz(    const wchar[] a);
//  baz(immutable wchar[] a);
int baz(          dchar[] a) { return 31; }
//  baz(    const dchar[] a);
//  baz(immutable dchar[] a);

int main()
{
    auto strn = "a";
    auto strc = "a"c;
    auto strw = "a"w;
    auto strd = "a"d;

    assert(foo("a" ) == 13);
    assert(foo(strn) == 13);
    assert(foo("a"c) == 13);
    assert(foo(strc) == 13);
    assert(foo("a"w) == 23);
    assert(foo(strw) == 23);
    assert(foo("a"d) == 33);
    assert(foo(strd) == 33);

    assert(bar("a" ) == 12);
    assert(bar(strn) == 12);
    assert(bar("a"c) == 12);
    assert(bar(strc) == 12);
    assert(bar("a"w) == 22);
    assert(bar(strw) == 22);
    assert(bar("a"d) == 32);
    assert(bar(strd) == 32);

    static assert(!__traits(compiles, baz("a" ) ));
    static assert(!__traits(compiles, baz(strn) ));
    static assert(!__traits(compiles, baz("a"c) ));
    static assert(!__traits(compiles, baz(strc) ));
    static assert(!__traits(compiles, baz("a"w) ));
    static assert(!__traits(compiles, baz(strw) ));
    static assert(!__traits(compiles, baz("a"d) ));
    static assert(!__traits(compiles, baz(strd) ));

    return 0;
}
