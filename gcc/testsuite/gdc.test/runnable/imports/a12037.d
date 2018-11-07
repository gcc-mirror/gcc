module imports.aXXXXX;

auto min(A, B)(A a, B b) { return a < b ? a : b; }
alias TypeTuple(T...) = T;

private template CustomFloatParams(uint bits)
{
    static if (bits ==  8) alias CustomFloatParams!( 4,  3) CustomFloatParams;
    static if (bits == 16) alias CustomFloatParams!(10,  5) CustomFloatParams;
    static if (bits == 32) alias CustomFloatParams!(23,  8) CustomFloatParams;
    static if (bits == 64) alias CustomFloatParams!(52, 11) CustomFloatParams;
    static if (bits == 80) alias CustomFloatParams!(64, 15) CustomFloatParams;
}
private template CustomFloatParams(uint precision, uint exponentWidth)
{
    alias TypeTuple!(
        precision,
        exponentWidth,
    ) CustomFloatParams;
}

struct CustomFloat(uint precision, uint exponentWidth)
if ((1 + precision + exponentWidth) % 8 == 0 && precision + exponentWidth > 0)
{
private:
    union ToBinary(F)
    if (is(typeof(CustomFloatParams!(F.sizeof*8))) || is(F == real))
    {
        F set;

        // If on Linux or Mac, where 80-bit reals are padded, ignore the
        // padding.
        CustomFloat!(CustomFloatParams!(min(F.sizeof*8, 80))) get;

        // Convert F to the correct binary type.
        static typeof(get) opCall(F value)
        {
            ToBinary r;
            r.set = value;
            return r.get;
        }
        alias get this;
    }

public:
    @property bool sign() { return 1; }
    @property void sign(bool) {}

    this(F)(F input)
    if (__traits(compiles, cast(real)input))
    {
        this = input;
    }

    void opAssign(F)(F input)
    if (__traits(compiles, cast(real)input))
    {
        static if (is(F == float) || is(F == double) || is(F == real))
                auto value = ToBinary!(F)(input);
        else    auto value = ToBinary!(real)(input);

        sign = value.sign;
    }

    @property F get(F)()
    if (is(F == float) || is(F == double) || is(F == real))
    {
        ToBinary!F result;
        return F.init;
    }

    T opCast(T)()
    if (__traits(compiles, get!T))
    {
        return get!T;
    }
}
