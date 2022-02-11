/************************************/
// https://issues.dlang.org/show_bug.cgi?id=9818

/*
TEST_OUTPUT:
---
sa1: [1, 1, 1]
ea1: [1, 1, 1]
sa2: [1, 1, 1]
ea2: [1, 1, 1]
eas: [1, 1, 1]
eac: [1, 1, 1]
sa3: [1, 1, 1]
ea3: [1, 1, 1]
sa4: [1, 1, 1]
ea4: [1, 1, 1]
---
*/

static const int[3] sa1 = 1;
pragma(msg, "sa1: ", sa1);          // doesn't work
static assert(sa1 == [1, 1, 1]);    // doesn't work

enum int[3] ea1 = 1;
pragma(msg, "ea1: ", ea1);          // prints "1" - bad
static assert(ea1 == [1, 1, 1]);    // doesn't work

struct X
{
    static const int[3] sa2 = 1;
    pragma(msg, "sa2: ", sa1);          // doesn't work
    static assert(sa2 == [1, 1, 1]);    // doesn't work

    enum int[3] ea2 = 1;
    pragma(msg, "ea2: ", ea2);          // prints "1" - bad
    static assert(ea2 == [1, 1, 1]);    // doesn't work
}

struct S
{
    enum int[3] eas = 1;
}
pragma(msg, "eas: ", S.eas);
static assert(S.eas == [1, 1, 1]);
class C
{
    enum int[3] eac = 1;
}
pragma(msg, "eac: ", C.eac);
static assert(C.eac == [1, 1, 1]);

void test()
{
    static const int[3] sa3 = 1;
    pragma(msg, "sa3: ", sa3);          // doesn't work
    static assert(sa3 == [1, 1, 1]);    // doesn't work

    enum int[3] ea3 = 1;
    pragma(msg, "ea3: ", ea3);          // prints "1" - bad
    static assert(ea3 == [1, 1, 1]);    // doesn't work

    struct Y
    {
        static const int[3] sa4 = 1;
        pragma(msg, "sa4: ", sa4);          // doesn't work
        static assert(sa4 == [1, 1, 1]);    // doesn't work

        enum int[3] ea4 = 1;
        pragma(msg, "ea4: ", ea4);          // prints "1" - bad
        static assert(ea4 == [1, 1, 1]);    // doesn't work
    }
}

/************************************/

void main() {}
