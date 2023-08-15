// https://issues.dlang.org/show_bug.cgi?id=23966
module test23966;

@("gigi")
void fun() {}
@("mimi")
void fun(int) {}
@("hihi")
void fun(int, int) {}
@("bibi")
void fun()(int, ulong) {}

void main()
{
    static foreach (t; __traits(getOverloads, test23966, "fun", true))
        static foreach(attr; __traits(getAttributes, t))
        {}

}
