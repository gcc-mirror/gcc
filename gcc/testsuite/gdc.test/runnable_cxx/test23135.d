// https://issues.dlang.org/show_bug.cgi?id=23135
// EXTRA_CPP_SOURCES: test23135.cpp
// REQUIRED_ARGS: -extern-std=c++11
// CXXFLAGS: -std=c++11
// DISABLED: win32

void main()
{
    test23135();
}

extern(C++):

void test23135();

class Mutable
{
    ~this();
    void func() { }
}

final class DeriveMutable : Mutable
{
    ~this();
    override void func() { }
}

class Const
{
    ~this();
    void func() const { }
}

final class DeriveConst : Const
{
    ~this();
    override void func() const { }
}
