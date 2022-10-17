/*
 * Test C++ abi-tag name mangling.
 * https://issues.dlang.org/show_bug.cgi?id=19949
 *
 * ABI tags are only supported on Linux & OSX,
 * however OSX doesn't use it in its standard library.
 *
 * Requires at minimum Clang 3.9.0 or GCC 5.1
 */

struct [[gnu::abi_tag("tag1")]] Tagged1 {};
struct [[gnu::abi_tag("tag2")]] Tagged2 {};

struct [[gnu::abi_tag("tag1", "tag2")]] Tagged1_2
{
    struct [[gnu::abi_tag("tag3")]] Tagged3
    {
        Tagged3(int value_) : value(value_) {}

        // _ZN9Tagged1_2B4tag1B4tag27Tagged3B4tag37Tagged4B4tag4Ev
        [[gnu::abi_tag("tag4")]]
        int Tagged4 () { return this->value; }

        int value;
    };
};
struct [[gnu::abi_tag("tag1")]] Tagged1Too {};

// _Z5inst1B4tag1B4tag2
Tagged1_2 inst1;
// _Z5inst2B4tag1B4tag2B4tag3
Tagged1_2::Tagged3 inst2(42);

// _Z5func0B4tag1B4tag2i
Tagged1_2 func0(int a) { return Tagged1_2(); }
// _Z5func19Tagged1_2B4tag1B4tag2
Tagged1_2 func1(Tagged1_2 a) { return a; }
// _Z5func2B4tag27Tagged1B4tag1
Tagged1_2 func2(Tagged1 a) { return Tagged1_2(); }
// _Z5func3B4tag17Tagged2B4tag2
Tagged1_2 func3(Tagged2 a) { return Tagged1_2(); }
// _Z5func47Tagged2B4tag27Tagged1B4tag1
Tagged1_2 func4(Tagged2 a, Tagged1 b) { return Tagged1_2(); }
// _Z5func5B4tag37Tagged2B4tag27Tagged1B4tag1
Tagged1_2::Tagged3 func5(Tagged2 a, Tagged1 b) { return Tagged1_2::Tagged3(420); }
// _Z5func67Tagged2B4tag2S_7Tagged1B4tag19Tagged1_2B4tag1B4tag2
void func6(Tagged2 a, Tagged2 b, Tagged1 c, Tagged1_2 d) {}
// With T=Tagged1_2: _Z5func7I9Tagged1_2B4tag1B4tag2ET_S1_i
template<typename T>
T func7(T a, int) { return a; }
// _Z5func87Tagged1B4tag110Tagged1TooB4tag1
void func8 (Tagged1, Tagged1Too) {}

// Explicitly instantiate the above templates.
template Tagged1_2 func7<Tagged1_2>(Tagged1_2, int);

struct [[gnu::abi_tag("foo", "bar")]] S
{
public:
    int i;
    S(int);
};

S::S(int i) : i(i) {}

[[gnu::abi_tag("foo", "bar")]]
int a;

S b(0);

[[gnu::abi_tag("foo", "bar")]]
int f() { return 0xf; }

S gs(int i) { return S(i + 0xe0); }
S gss(S s, int i) { return S(i + s.i + 0xe0); }

[[gnu::abi_tag("foo", "bar")]]
S fss(S s, int i) { return S(i + s.i + 0xf); }

template <class T>
T gt(int i) { return T(i + 0xe0); }

template <class T>
T gtt(T t, int i) { return T(i + t.i + 0xe0); }


template <class T>
[[gnu::abi_tag("foo", "bar")]] /* GCC is inconsistent here, <= 6 matches clang but >= 7 is different */
T ft(int i) { return T(i + 0xf); }

template <class T>
[[gnu::abi_tag("foo", "bar")]] /* GCC is inconsistent here, <= 6 matches clang but >= 7 is different */
T ftt(T t, int i) { return T(i + t.i + 0xf); }

#ifdef __clang__
inline namespace [[gnu::abi_tag("AAA")]] N
#else
inline namespace N [[gnu::abi_tag("AAA")]]
#endif
{
    template <int>
    struct [[gnu::abi_tag("foo", "bar")]] K
    {
    public:
        int i;
        K(int i);
    };
}

template <int j>
K<j>::K(int i) : i(i) {}

// Note: This does not include the 'AAA' in the mangling
// template <int j>
// K<j> fk(int i) { return K<j>(i + j + 0xf); }

K<1> fk1(int i) { return K<1>(i + 1 + 0xf); }

K<10> k10(0);

void initVars()
{
    a = 10;
    b = S(20);
    k10 = K<10>(30);
}

// doesn't compile on GCC < 6, and not at all on Clang (tested with 9.0.1)
#if __GNUC__ >= 6
enum [[gnu::abi_tag("ENN")]] E0
{ E0a = 0xa };

E0 fe() { return E0a; }

template<int>
E0 fei() { return E0a; }
#endif

// Explicitly instantiate the above templates.
template S gt<S>(int);
template S gtt<S>(S, int);
template S ft<S>(int);
template S ftt<S>(S, int);
#if __GNUC__ >= 6
template E0 fei<0>();
#endif
