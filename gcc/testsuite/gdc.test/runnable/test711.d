// https://issues.dlang.org/show_bug.cgi?id=711
string result;

template Mixer()
{
    override void test()
    {
        result ~= "A";
    }
}

class Foo
{
    void test()
    {
        result ~= "B";
    }
}

class Bar : Foo
{
    mixin Mixer!() mixer;
    override void test()
    {
        result ~= "C";
        mixer.test();
    }
}

class Bar2 : Foo
{
    override void test()
    {
        result ~= "C";
        mixer.test();
    }
    mixin Mixer!() mixer;
}

void main()
{
    Bar f = new Bar();
    f.test();
    assert(result == "CA");

    result = "";

    Bar2 f2 = new Bar2();
    f2.test();
    assert(result == "CA");
}
