// https://issues.dlang.org/show_bug.cgi?id=23142

struct Foo
{
    int x;

scope:
    void func()
    {
    }

    unittest
    {
    }

    static void func2()
    {
    }
}
