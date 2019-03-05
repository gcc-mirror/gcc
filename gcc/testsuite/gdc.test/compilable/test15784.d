// PERMUTE_ARGS:

template AddField(T)
{
    T b;
    this(Args...)(T b, auto ref Args args)
    {
        this.b = b;
        this(args);
    }
}

template construcotrs()
{
    int a;
    this(int a)
    {
        this.a = a;
    }
}

class B
{
    mixin construcotrs;
    mixin AddField!(string);
}

class C : B
{
    this(A...)(A args)
    {
        // The called super ctor is an overload set.
        super(args);
    }
}

struct S
{
    mixin construcotrs;
    mixin AddField!(string);
}

void main()
{
    auto s = S("bar", 15);
    auto c = new C("bar", 15);
}
