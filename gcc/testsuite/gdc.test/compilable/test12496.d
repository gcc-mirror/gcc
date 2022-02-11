// https://issues.dlang.org/show_bug.cgi?id=12496

final abstract class T1
{
    final abstract class C(uint value) { }

    alias Child = C!2;
}

void main()
{
    static assert(__traits(isSame, __traits(parent, T1.Child), T1));
}
