class Base
{
    this() shared
    {}

    this()
    {}
}

class Derived1 : Base
{
    this()
    {
        // implicit super();
    }
}

class Derived2 : Base
{
    // implicit this()
}

class Base2
{
    this() shared
    {}
}

class Derived3 : Base2
{
    // implicit this() shared
}

void test()
{
    auto d2 = new Derived2;
    auto d3 = new shared(Derived3);
}
