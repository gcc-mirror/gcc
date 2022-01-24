/*
TEST_OUTPUT:
---
fail_compilation/fail14304.d(26): Error: reinterpreting cast from `const(S14304)*` to `S14304*` is not supported in CTFE
fail_compilation/fail14304.d(58):        called from here: `sle14304.modify()`
fail_compilation/fail14304.d(35): Error: cannot modify read-only constant `[1:1, 2:2]`
fail_compilation/fail14304.d(61):        called from here: `modify14304(aae14304)`
fail_compilation/fail14304.d(41): Error: cannot modify read-only constant `[1, 2, 3]`
fail_compilation/fail14304.d(64):        called from here: `modify14304(cast(const(int)[])index14304)`
fail_compilation/fail14304.d(46): Error: array cast from `immutable(double[])` to `double[]` is not supported at compile time
fail_compilation/fail14304.d(67):        called from here: `modify14304(cast(const(double)[])slice14304)`
fail_compilation/fail14304.d(53): Error: cannot modify read-only string literal `"abc"`
fail_compilation/fail14304.d(70):        called from here: `modify14304(cast(const(char)[])str14304)`
---
*/

struct S14304
{
    int x;

    int modify() const
    {
        assert(x == 1);

        // This force modification must not affect to ghe s14304 value.
        (cast(S14304*)&this).x = 10;

        assert(x == 10);
        return x;
    }
}
int modify14304(immutable int[int] aa)
{
    auto p = cast(int*)&aa[1];
    *p = 10;
    return aa[1];
}
int modify14304(const(int)[] arr)
{
    auto a = cast(int[])arr;
    a[0] = 10;
    return arr[0];
}
int modify14304(const(double)[] arr)
{
    auto a = cast(double[])arr;
    a[] = 3.14;
    return cast(int)arr[0];
}
int modify14304(const(char)[] str)
{
    auto s = cast(char[])str;
    s[0] = 'z';
    return str[0];
}

static immutable sle14304 = immutable S14304(1);
static immutable v14304 = sle14304.modify();

static immutable aae14304 = [1:1, 2:2];
static immutable w14304 = modify14304(aae14304);

static immutable index14304 = [1, 2, 3];
static immutable x14304 = modify14304(index14304);

static immutable slice14304 = [1.414, 1.732, 2];
static immutable y14304 = modify14304(slice14304);

static immutable str14304 = "abc";
static immutable z14304 = modify14304(str14304);
