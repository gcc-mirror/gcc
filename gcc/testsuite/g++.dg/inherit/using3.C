class A
{
public:
    typedef int T;
    int a;
};

class B : virtual private A
{
};

class C : virtual private A, public B
{
public:
    using A::a;
    using A::T;
};

C::T x;
