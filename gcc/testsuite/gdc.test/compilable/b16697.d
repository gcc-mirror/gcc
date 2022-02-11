static assert(!is(         float     == __vector));
static assert(!is(         float[1]  == __vector));
static assert(!is(         float[4]  == __vector));
static assert(!is(__vector(float[3]) == __vector));
static assert(!is(__vector(float[5]) == __vector));

static if (__traits(compiles, __vector(float[4])))
{
    static assert(is(__vector(float[4])   == __vector));
    static assert(is(__vector(float[4]) X == __vector) &&
                  is(X == float[4]));
}

static if (__traits(compiles, __vector(byte[16])))
{
    static assert(is(__vector(byte[16]) X == __vector) &&
                  is(X == byte[16]));
}
