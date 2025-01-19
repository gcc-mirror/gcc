/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test20245.d(21): Error: assigning reference to local variable `x` to non-scope parameter `ptr` calling `escape` is not allowed in a `@safe` function
fail_compilation/test20245.d(22): Error: escaping a reference to parameter `x` by copying `&x` into allocated memory is not allowed in a `@safe` function
fail_compilation/test20245.d(23): Error: returning scope variable `a` is not allowed in a `@safe` function
fail_compilation/test20245.d(27): Error: taking address of `scope` variable `x` with pointers is not allowed in a `@safe` function
fail_compilation/test20245.d(33): Error: assigning reference to local variable `x` to non-scope parameter `ptr` calling `escape` is not allowed in a `@safe` function
fail_compilation/test20245.d(34): Error: escaping a reference to parameter `x` by copying `&x` into allocated memory is not allowed in a `@safe` function
fail_compilation/test20245.d(50): Error: assigning reference to local variable `price` to non-scope `this.minPrice` is not allowed in a `@safe` function
fail_compilation/test20245.d(69): Error: reference to local variable `this.content[]` calling non-scope member function `Exception.this()` is not allowed in a `@safe` function
fail_compilation/test20245.d(89): Error: assigning reference to local variable `this` to non-scope parameter `content` calling `listUp` is not allowed in a `@safe` function
fail_compilation/test20245.d(82):        which is not `scope` because of `charPtr = content`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20245
@safe int* foo(ref int x) {
    int* a = &x;
    escape(&x);
    auto b = [&x];
    return a;
}

@safe int** foo(ref scope int* x) {
    int** a = &x;
    return a;
}

@safe int* foo(return ref int x) {
    int* a = &x;
    escape(&x);
    auto b = [&x];
    return a;
}

int* gPtr;
@safe void escape(int* ptr)
{
    gPtr = ptr;
}

// https://issues.dlang.org/show_bug.cgi?id=21212
class MinPointerRecorder
{
    int* minPrice;
    void update(ref int price) @safe
    {
        minPrice = &price; // Should not compile.
    }
}

void main() @safe
{
    auto r = new MinPointerRecorder;
    () { int mp = 42; r.update(mp); } ();
    () { ulong[1000] stomp = 13; } ();
    auto x = *r.minPrice; // "13"
}

// https://issues.dlang.org/show_bug.cgi?id=22782
struct DontDoThis
{
    immutable char[12] content;
    @safe this(char ch)
    {
        content[] = ch;
        throw new Exception(content[]);
    }
}

void main1() @safe
{
    DontDoThis('a');
}

// https://issues.dlang.org/show_bug.cgi?id=22783
const(char)* charPtr;

// argument is not, or should not be scope
auto listUp(const(char)* content) {charPtr = content;}

struct DontDoThis2
{
    char content;
    @safe escape()
    {
        listUp(&content);
    }
}
