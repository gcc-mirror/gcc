
mixin template X8937()
{
    int value;
}

debug = test;

void main()
{
    // (static) import statement
    {
        static assert(!__traits(compiles, cos(0)));
        if (true)
        {
            static assert(!__traits(compiles, cos(0)));
            import core.stdc.math;
            static assert( __traits(compiles, cos(0)));
        }
        static assert(!__traits(compiles, cos(0)));

        if (true)
            import core.stdc.math;
        static assert(!__traits(compiles, cos(0))); // fails

        if (true)
            static import core.stdc.math;
        static assert(!__traits(compiles, core.stdc.math.cos(0))); // fails
    }
    static assert(!__traits(compiles, cos(0)));

    // mixin statement
    {
        if (true)
            mixin X8937!();
        static assert(!__traits(compiles, value)); // fails
    }

    // enum declaration
    {
        if (true)
            enum E { x = 10 }
        static assert(!__traits(compiles, E)); // fails
    }

    // conditional declarations
    {
        if (true)
            static if (true) struct S1 {}
        static assert(!__traits(compiles, S1)); // fails

        if (true)
            version (all) struct S2 {}
        static assert(!__traits(compiles, S2)); // fails

        if (true)
            debug (test) struct S3 {}
        static assert(!__traits(compiles, S3)); // fails
    }
}
