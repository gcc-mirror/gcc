class A
{
    this() {}
}

class B : A
{
    this()
    {
        static if (__traits(compiles, super()))
            super();
    }
}
