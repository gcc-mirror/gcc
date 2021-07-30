// https://issues.dlang.org/show_bug.cgi?id=22133

struct Slice
{
    bool empty() const;
    int front() const;
    void popFront()() // note: requires a mutable Slice
    {}
}

enum isInputRange1(R) = is(typeof((R r) => r.popFront));
enum isInputRange2(R) = __traits(compiles, (R r) => r.popFront);
static assert(isInputRange1!(      Slice) == true);
static assert(isInputRange1!(const Slice) == false);
static assert(isInputRange2!(      Slice) == true);
static assert(isInputRange2!(const Slice) == false);
