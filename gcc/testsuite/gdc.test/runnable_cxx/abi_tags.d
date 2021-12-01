/*
 * Test C++ abi-tag name mangling.
 * They are a C++11 feature required to bind `std::string`,
 * introduced in G++ 5.1 / clang++ 3.9
 * https://issues.dlang.org/show_bug.cgi?id=19949
 *
 * DISABLED: win32 win64
 * REQUIRED_ARGS: -extern-std=c++11
 * EXTRA_CPP_SOURCES: abi_tags.cpp
 * CXXFLAGS: -std=c++11
 */

#line 100
import core.attribute;

alias Tuple(A...) = A;
enum foo_bar = gnuAbiTag("foo", "bar");

extern(C++)
{
    @gnuAbiTag("tag1") struct Tagged1 {}
    @gnuAbiTag("tag2") struct Tagged2 {}
    @gnuAbiTag("tag1") struct Tagged1Too {}

    // Note: Outer tags do not propagate, unlike in C++
    @gnuAbiTag("tag1", "tag2") struct Tagged1_2
    {
        @gnuAbiTag("tag1", "tag2", "tag3") struct Tagged3
        {
            // _ZN9Tagged1_2B4tag1B4tag27Tagged3B4tag37Tagged4B4tag4Ev
            @gnuAbiTag("tag1", "tag2", "tag3", "tag4") int Tagged4 ();

            int value;
        }
    }

    extern __gshared Tagged1_2 inst1;
    extern __gshared Tagged1_2.Tagged3 inst2;

    Tagged1_2 func0(int a);
    Tagged1_2 func1(Tagged1_2 a);
    Tagged1_2 func2(Tagged1 a);
    Tagged1_2 func3(Tagged2 a);
    Tagged1_2 func4(Tagged2 a, Tagged1 b);
    Tagged1_2.Tagged3 func5(Tagged2 a, Tagged1 b);
    void func6(Tagged2 a, Tagged2 b, Tagged1 c, Tagged1_2 d);
    T func7(T)(T a, int);
    void func8 (Tagged1, Tagged1Too);

    @foo_bar struct S
    {
        int i;
    }

    @foo_bar extern __gshared int a;

    extern __gshared S b;

    @foo_bar int f();

    S gs(int);
    S gss(S, int);

    @foo_bar S fss(S, int);

    T gt(T)(int);
    T gtt(T)(T, int);

    @foo_bar T ft(T)(int);

    @foo_bar T ftt(T)(T, int);

    @("abc") extern(C++, "N")
    {
        @gnuAbiTag("AAA", "foo")
        template K(int i)
        {
            @gnuAbiTag("bar", "AAA", "foo")
            struct K
            {
                int i;
                this(int);
            }
        }
    }

    //K!i fk(int i)(int);
    K!1 fk1(int);

    extern __gshared K!10 k10;

    @gnuAbiTag("ENN") enum E0 { a = 0xa, }
    E0 fe();
    E0 fei(int i)();

    void initVars();
}

void main()
{
    inst1 = func0(42);
    assert(inst2.value == 42);
    inst2 = func5(Tagged2.init, Tagged1.init);
    assert(inst2.value == 420);
    func1(inst1);
    func2(Tagged1.init);
    func3(Tagged2.init);
    func4(Tagged2.init, Tagged1.init);

    func6(Tagged2.init, Tagged2.init, Tagged1.init, Tagged1_2.init);
    func7(Tagged1_2.init, 42);
    func8(Tagged1.init, Tagged1Too.init);

    initVars();
    assert(a == 10);
    assert(b.i == 20);
    assert(k10.i == 30);

    assert(f() == 0xf);
    assert(gs(1).i == 1+0xe0);
    assert(gss(S(1), 1).i == 2+0xe0);
    assert(fss(S(1), 1).i == 2+0xf);
    assert(gt!S(1).i == 1+0xe0);
    assert(gtt!S(S(1), 1).i == 2+0xe0);

    // Bug: Template parameter tags get double mangled
    version(none)
    {
        assert(ft!S(1).i == 1+0xf);        // GCC inconsistent
        assert(ftt!S(S(1), 1).i == 2+0xf); // GCC inconsistent
    }
    //assert(fk!0(1).i == 1+0xf);
    assert(fk1(1).i == 2+0xf);
    version(gcc6)
    {
        assert(fei!0() == E0.a); // GCC only
        assert(fe() == E0.a);    // GCC only
    }
}
