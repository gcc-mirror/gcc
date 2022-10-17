// https://issues.dlang.org/show_bug.cgi?id=21876

auto test1()
{
    int[0] a;
    return a;
}

auto test2()
{
    static int[0] a;
    return a;
}

enum x = test1();
enum y = test2();
static assert(x == y && y == []);
