// NOTE: comes from std.bitmanip

// PERMUTE_ARGS:

private string myToString(ulong n)
{
    return n < 10
        ? "" ~ cast(char) (n + '0')
        : myToString(n / 10) ~ myToString(n % 10);
}

private string toStringSfx(ulong n)
{
    return myToString(n) ~ (n > uint.max ? "UL" : "U");
}

private string CreateAccessors(
    string store, T, string name, size_t len, size_t offset)()
{
    static if (!name.length)
    {
        // No need to create any accessor
        return "";
    }
    else
    {
        static const
            maskAllElse = ((1uL << len) - 1u) << offset,
            maskMyself = ~maskAllElse,
            signBitCheck = 1uL << (len - 1),
            extendSign = ~((1uL << len) - 1);

        string result;
        static if (is(T == bool))
        {
            static assert(len == 1);
            // getter
            result ~= "bool " ~ name ~ "(){ return "
                "("~store~" & "~toStringSfx(maskAllElse)~") != 0;}";
            // setter
            result ~= "void " ~ name ~ "(bool v){"
                "if (v) "~store~" |= "~toStringSfx(maskAllElse)~";"
                "else "~store~" &= "~toStringSfx(maskMyself)~";}";
        }
        else
        {
            // getter
            result ~= T.stringof ~ " " ~ name ~ "(){ auto result = "
                "("~store~" & "
                ~ toStringSfx(maskAllElse) ~ ") >>"
                ~ toStringSfx(offset) ~ ";";
            static if (T.min < 0)
            {
                result ~= "if (result >= " ~ toStringSfx(signBitCheck)
                    ~ ") result |= " ~ toStringSfx(extendSign) ~ ";";
            }
            result ~= " return cast(" ~ T.stringof ~ ") result;}";
            // setter
            result ~= "void " ~ name ~ "(" ~ T.stringof
                ~ " v){ "~store~" = ("~store~" & "
                ~ toStringSfx(maskMyself) ~ ") | "
                ~ "((cast(typeof("~store~")) v << " ~ toStringSfx(offset)
                ~ ") & " ~ toStringSfx(maskAllElse)
                ~ ");}";
        }
        return result;
    }
}

private string createStoreName(Ts...)()
{
    static if (Ts.length == 0)
        return "";
    else
        return Ts[1] ~ createStoreName!(Ts[3 .. $])();
}

private string CreateFields(string store, size_t offset, Ts...)()
{
    static if (!Ts.length)
    {
        static if (offset == ubyte.sizeof * 8)
            return "private ubyte " ~ store ~ ";";
        else static if (offset == ushort.sizeof * 8)
            return "private ushort " ~ store ~ ";";
        else static if (offset == uint.sizeof * 8)
            return "private uint " ~ store ~ ";";
        else static if (offset == ulong.sizeof * 8)
            return "private ulong " ~ store ~ ";";
        else
            static assert(false, ToString!(offset));
    }
    else
    {
        return CreateAccessors!(store, Ts[0], Ts[1], Ts[2], offset)()
            ~ CreateFields!(store, offset + Ts[2], Ts[3 .. $])();
    }
}

template BitFields(T...)
{
    //pragma(msg, CreateFields!(createStoreName!(T)(), 0, T)());
    mixin(CreateFields!(createStoreName!(T)(), 0, T)());
}

struct FloatRep
{
    mixin BitFields!(
        uint, "fraction", 23,
        uint, "exponent", 8,
        bool, "sign", 1);
}

struct DoubleRep
{
    mixin BitFields!(
        ulong, "fraction", 52,
        uint, "exponent", 11,
        bool, "sign", 1);
}

struct Bug2355(string x) {}
static assert(is(Bug2355!"a" U : Bug2355!V, string V));

int main()
{
    return 0;
}
