// https://issues.dlang.org/show_bug.cgi?id=23567
extern(C++) abstract class CCvar
{
public:
    pragma(printf) void func1(const(char)* pFormat, ...);
    pragma(printf) void func2(const(char)* pFormat, ...);
}

static assert(__traits(getVirtualIndex, CCvar.func2) == 1);
