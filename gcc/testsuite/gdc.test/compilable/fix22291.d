//https://issues.dlang.org/show_bug.cgi?id=22291

alias AliasSeq(T...) = T;
void noParameters()
{
    static assert(typeof(__traits(parameters)).length == 0);
}
void noArgs()
{
    //Arguments are not valid, this should not compile
    static assert(!__traits(compiles, __traits(parameters, 456)));
}
shared static this()
{
    static assert(typeof(__traits(parameters)).length == 0);
}
int echoPlusOne(int x)
{
    __traits(parameters)[0] += 1;
    return x;
}
static assert(echoPlusOne(1) == 2);
class Tree {
    int opApply(int delegate(size_t, Tree) dg) {
        if (dg(0, this)) return 1;
        return 0;
    }
}
void useOpApply(Tree top, int x)
{
    foreach(idx; 0..5)
    {
        static assert(is(typeof(__traits(parameters)) == AliasSeq!(Tree, int)));
    }
    foreach(idx, elem; top)
    {
        static assert(is(typeof(__traits(parameters)) == AliasSeq!(size_t, Tree)));
    }
}
class Test
{
    static assert(!__traits(compiles, __traits(parameters)));
    void handle(int x)
    {
        static assert(typeof(__traits(parameters)).length == 1);
    }
}

int add(int x, int y)
{
	return x + y;
}

auto forwardToAdd(int x, int y)
{
	return add(__traits(parameters));
}
static assert(forwardToAdd(2, 3) == 5);
struct TestConstructor
{
    int x;
    string y;
    //This parameter will not have a name but it's (tuple) members
    //will
    this(typeof(this.tupleof))
    {
        this.tupleof = __traits(parameters);
    }
}
bool test(int x, string y)
{
    auto s = TestConstructor(2, "pi");
    return s.x == x && s.y == y;
}
static assert(test(2, "pi"));
int testNested(int x)
{
    static assert(typeof(__traits(parameters)).length == 1);
    int add(int x, int y)
    {
        static assert(typeof(__traits(parameters)).length == 2);
        return x + y;
    }
    return add(x + 2, x + 3);
}
void testPack(Pack...)(Pack x)
{
    static assert(is(typeof(__traits(parameters)) == typeof(AliasSeq!(x))));
}

ref int forwardTest(return ref int x)
{
    static assert(__traits(isRef, x) == __traits(isRef, __traits(parameters)[0]));
    return x;
}

int testRefness(int x, ref int monkey)
{
    {
        //monkey = x;
        __traits(parameters)[1] = __traits(parameters)[0];
    }
    return x;
}
int refTest()
{
    int x;
    testRefness(45, x);
    return x;
}
auto packLength(Pack...)(Pack x)
{
    return typeof(__traits(parameters)).length;
}
static assert(packLength(2, 3) == 2);
alias lambda = (x) => typeof(__traits(parameters)).stringof;
static assert(lambda(1) == "(int)");
static assert(refTest() == 45);

T testTemplate(T)(scope T input)
{
    void chimpInASuit(float set)
    {
        static assert(is(typeof(__traits(parameters)) == AliasSeq!(float)));
    }
    {
        __traits(parameters) = AliasSeq!(T.max);
    }
    __traits(parameters) = AliasSeq!(T.init);
    return input;
}

static assert(testTemplate!long(420) == 0);

