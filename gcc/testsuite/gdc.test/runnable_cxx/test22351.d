// https://issues.dlang.org/show_bug.cgi?id=22351
// EXTRA_CPP_SOURCES: test22351.cpp
// REQUIRED_ARGS: -extern-std=c++11
// CXXFLAGS: -std=c++11
// DISABLED: win32

extern(C++) class A22351
{
    int f()
    {
        return 1;
    }

    int g(int*)
    {
        return 3;
    }

    int h()
    {
        return 5;
    }

    int h() const
    {
        return 7;
    }
}

extern(C++) class B22351 : A22351
{
    alias f = A22351.f;
    alias g = A22351.g;
    alias h = A22351.h;

    int f() const
    {
        return 2;
    }

    int g(const(int)*)
    {
        return 4;
    }

    override int h() const
    {
        return 6;
    }
}

extern(C++) B22351 createB()
{
    return new B22351;
}
