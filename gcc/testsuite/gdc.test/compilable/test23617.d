// https://issues.dlang.org/show_bug.cgi?id=23617

struct S
{
    void foo() {}
}

struct Wrapper
{
    size_t currentIndex;
    S[] arrayOfS;

    auto opDispatch(string name, T ...)(T t)
    {
        return __traits(child, arrayOfS[this.currentIndex], __traits(getMember, S, name))(t);
    }
}

void main()
{
        Wrapper w;
        w.opDispatch!"foo"();
}
