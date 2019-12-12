interface ITest
{
    int foo();

    final void bar(int k)() { assert(foo() == k); }
}

class Test : ITest
{
    override int foo() { return 12; }
}

void main()
{
    auto test = new Test;
    test.bar!12();
}
