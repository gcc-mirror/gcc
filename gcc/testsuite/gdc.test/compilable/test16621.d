// https://issues.dlang.org/show_bug.cgi?id=16621

template xxx()
{
    Vector2f xxx()
    {
	 return this;
    }
}

struct Vector2f
{
    mixin xxx!();
    alias xxx this;
}

void foo(ref Vector2f pos);

void test()
{
    Vector2f v;
    foo(v);
}
