// https://issues.dlang.org/show_bug.cgi?id=18871
// and https://issues.dlang.org/show_bug.cgi?id=18819

struct Problem
{
    ~this() {}
}
struct S
{
    Problem[1] payload;
}
enum theTemplateB = {
    static foreach (e; S.init.tupleof) {}
    return true;
}();
