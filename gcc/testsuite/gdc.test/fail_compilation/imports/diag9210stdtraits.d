module imports.diag9210stdtraits;

template FloatingPointTypeOf(T)// if (!is(T == enum))
{
           inout( float) idx(        inout( float) );
           inout(double) idx(        inout(double) );
           inout(  real) idx(        inout(  real) );
    shared(inout  float) idx( shared(inout  float) );
    shared(inout double) idx( shared(inout double) );
    shared(inout   real) idx( shared(inout   real) );

       immutable( float) idy(   immutable( float) );
       immutable(double) idy(   immutable(double) );
       immutable(  real) idy(   immutable(  real) );

    static if (is(typeof(idx(T.init)) X))
    {
        alias X FloatingPointTypeOf;
    }
    else static if (is(typeof(idy(T.init)) X))
    {
        alias X FloatingPointTypeOf;
    }
    else
    {
        static assert(0, T.stringof~" is not a floating point type");
    }
}

template isFloatingPoint(T)
{
    enum bool isFloatingPoint = is(FloatingPointTypeOf!T);
}
