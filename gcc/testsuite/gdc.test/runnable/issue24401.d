// PERMUTE_ARGS:
// https://issues.dlang.org/show_bug.cgi?id=24401
int main()
{
    return (() @trusted => 0)();
}
