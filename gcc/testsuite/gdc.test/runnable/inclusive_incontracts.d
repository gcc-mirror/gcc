// REQUIRED_ARGS: -preview=inclusiveincontracts
// PERMUTE_ARGS: -O -inline

import core.exception : AssertError;

void main()
{
    basic_test;
    multiple_incontracts_test;
}

void basic_test()
{
    class A
    {
        void foo() @nogc
        in (true, "foo")
        {
        }
    }

    class B : A
    {
        override void foo() @nogc
        in (false, "nope not foo")
        {
        }
    }

    auto b = new B;

    try
    {
        b.foo;
        throw new Exception("Assert expected.");
    }
    catch (AssertError assertError)
    {
        assert(assertError.line == 25); // line of invalid in-contract
        assert(assertError.msg == "Logic error: in-contract was tighter than parent in-contract");
    }
}

void multiple_incontracts_test()
{
    class A
    {
        void foo(int a, int b)
        in (a > 0, "A::a")
        in (b > 0, "B::b")
        {
        }
    }

    class B : A
    {
        override void foo(int a, int b)
        in (a >= 0, "B::a")
        in (b > 0, "B::b")
        {
        }
    }

    auto b = new B;

    b.foo(0, 2);
    try
    {
        b.foo(0, 0);
        throw new Exception("Assert expected.");
    }
    catch (AssertError assertError) {
        /**
         * Having found that the looser contract in B is not fulfilled, we try
         * the stricter contract in A. As that one is also violated, we error.
         */
        assert(assertError.line == 49);
        assert(assertError.msg == "A::a");
    }
}
