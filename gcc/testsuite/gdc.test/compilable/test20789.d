// REQUIRED_ARGS: -de
module compilable.test20789;

struct S(bool deprecateFunction, bool deprecateAlias)
{
    static if (deprecateFunction)
        deprecated string get() { return "foo"; }
    else
        string get() { return "foo"; }

    static if (deprecateAlias)
        deprecated alias get this;
    else
        alias get this;
}

void main()
{
    void normalFun()
    {
        static assert( is(S!(false, false) : string));
        static assert(!is(S!(false, true ) : string));
        static assert(!is(S!(true , false) : string));
        static assert(!is(S!(true , true ) : string));
    }
    deprecated void deprecatedFun()
    {
        // deprecations are allowed in a deprecated scope.
        static assert(is(S!(false, false) : string));
        static assert(is(S!(false, true ) : string));
        static assert(is(S!(true , false) : string));
        static assert(is(S!(true , true ) : string));
    }
}
