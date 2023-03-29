/*
REQUIRED_ARGS: -w -o- -d

More complex examples from the DIP
https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1034.md
*/

alias noreturn = typeof(*null);
static assert (!is(noreturn == void));

void initialize()
{
    noreturn a;
    noreturn b = noreturn.init;
}

void foo(const noreturn);
void foo(const int);

noreturn bar();

void overloads()
{
    noreturn n;
    foo(n);

    foo(bar());
}

// /*****************************************************************************/

auto inferNoreturn(int i)
{
    if (i < 0)
        return assert(false);
    else if (i == 0)
        return assert(false);
    else
        return assert(false);
}

auto inferReturn(int i)
{
    if (i < 0)
        return assert(false);
    else if (i == 0)
        return i;
    else
        return assert(false);
}

// /*****************************************************************************/
// // https://issues.dlang.org/show_bug.cgi?id=22004

alias fun22004 = _ => {}();
alias gun22004 = _ => assert(0);
auto bun22004(bool b)
{
    if (b)
        return gun22004(0);
    else
        return fun22004(0);
}

static assert(is(typeof(bun22004(true)) == void));

// // Reversed order
auto bun22004_reversed(bool b)
{
    if (b)
        return fun22004(0);
    else
        return gun22004(0);
}

static assert(is(typeof(bun22004_reversed(true)) == void));

// /*****************************************************************************/

// // Also works fine with non-void types and ref inference

int global;

auto ref forwardOrExit(ref int num)
{
    if (num)
        return num;
    else
        return assert(false);
}

static assert( is(typeof(forwardOrExit(global)) == int));

// // Must not infer ref due to the noreturn rvalue
static assert(!is(typeof(&forwardOrExit(global))));

auto ref forwardOrExit2(ref int num)
{
    if (num)
        return assert(false);
    else
        return num;
}

static assert( is(typeof(forwardOrExit2(global)) == int));

// // Must not infer ref due to the noreturn rvalue
static assert(!is(typeof(&forwardOrExit2(global))));

/*****************************************************************************/

void inference()
{
    auto inf = cast(noreturn) 1;
    static assert(is(typeof(inf) == noreturn));

    noreturn n;
    auto c = cast(const shared noreturn) n;
    static assert(is(typeof(c) == const shared noreturn));
    static assert(is(typeof(n) == noreturn));

    auto c2 = cast(immutable noreturn) n;
    static assert(is(typeof(c) == const shared noreturn));
    static assert(is(typeof(c2) == immutable noreturn));
    static assert(is(typeof(n) == noreturn));
}


/******************************************************************************/
// https://issues.dlang.org/show_bug.cgi?id=21957
// Calculate proper alignment and size for noreturn members

enum longPad = long.alignof - int.sizeof;

struct BasicStruct
{
	int firstInt;
	noreturn noRet;
	long lastLong;
}

static assert(BasicStruct.sizeof == (int.sizeof + longPad + long.sizeof));

static assert(BasicStruct.firstInt.offsetof == 0);
static assert(BasicStruct.noRet.offsetof == 4);
static assert(BasicStruct.lastLong.offsetof == (4 + longPad));

struct AlignedStruct
{
	int firstInt;
	align(16) noreturn noRet;
	long lastLong;
}

static assert(AlignedStruct.sizeof == 32);

static assert(AlignedStruct.firstInt.offsetof == 0);
static assert(AlignedStruct.noRet.offsetof == 16);
static assert(AlignedStruct.lastLong.offsetof == 16);

union BasicUnion
{
	int firstInt;
	noreturn noRet;
	long lastLong;
}

static assert(BasicUnion.sizeof == 8);

static assert(BasicUnion.firstInt.offsetof == 0);
static assert(BasicUnion.noRet.offsetof == 0);
static assert(BasicUnion.lastLong.offsetof == 0);

union AlignedUnion
{
	int firstInt;
	align(16) noreturn noRet;
	long lastLong;
}

static assert(AlignedUnion.sizeof == 16);

static assert(AlignedUnion.firstInt.offsetof == 0);
static assert(AlignedUnion.noRet.offsetof == 0);
static assert(AlignedUnion.lastLong.offsetof == 0);

class BasicClass
{
	int firstInt;
	noreturn noRet;
	long lastLong;
}

enum objectMemberSize = __traits(classInstanceSize, Object);

static assert(__traits(classInstanceSize, BasicClass) == objectMemberSize + (int.sizeof + longPad + long.sizeof));

static assert(BasicClass.firstInt.offsetof == objectMemberSize + 0);
static assert(BasicClass.noRet.offsetof == objectMemberSize + 4);
static assert(BasicClass.lastLong.offsetof == objectMemberSize + (4 + longPad));

class AlignedClass
{
	int firstInt;
	align(16) noreturn noRet;
	long lastLong;
}

enum offset = (objectMemberSize + 4 + 16) & ~15;

static assert(__traits(classInstanceSize, AlignedClass) == offset + 8);

static assert(AlignedClass.firstInt.offsetof == objectMemberSize + 0);
static assert(AlignedClass.noRet.offsetof == offset);
static assert(AlignedClass.lastLong.offsetof == offset);

struct EmptyStruct
{
	noreturn noRet;
}

static assert(EmptyStruct.sizeof == 1);
static assert(EmptyStruct.noRet.offsetof == 0);

struct EmptyStruct2
{
	noreturn[4] noRet;
}

static assert(EmptyStruct2.sizeof == 1);
static assert(EmptyStruct2.noRet.offsetof == 0);

// https://issues.dlang.org/show_bug.cgi?id=22858
// Shouldn't mess with the alignment of other zero-sized types.

struct S22858
{
    int a;
    void*[0] arr;
    char c;
    noreturn[0] arr2;
    char c2;
}

static assert (S22858.arr.offsetof % size_t.sizeof == 0);
static assert (S22858.arr2.offsetof == S22858.c.offsetof + 1);
static assert (S22858.arr2.offsetof == S22858.c2.offsetof);
