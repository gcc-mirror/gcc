// https://issues.dlang.org/show_bug.cgi?id=22130

int* f(const int* input) pure nothrow @safe
{
    int* output;
    return output;
}
void main() pure nothrow @safe
{
    int* c = new int;
    immutable int* i = f(c);
}
