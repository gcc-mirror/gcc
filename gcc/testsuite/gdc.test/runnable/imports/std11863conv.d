module imports.std11863conv;

import imports.std11863format;

template to(T)
{
    T to(A...)(A args)
    {
        return toImpl!T(args);
    }
}

T toImpl(T, S)(S value)
{
    static if (is(S == int) && is(T == string))
    {
        // other integral-to-string conversions with default radix
        return toImpl!(T, S)(value, 10);
    }
    else
        static assert(0);
}

@trusted pure T toImpl(T, S)(S value, uint radix/*, LetterCase letterCase = LetterCase.upper*/)
{
    static assert(is(S == int) && is(T == string));

    alias EEType = char/*Unqual!(typeof(T.init[0]))*/;

    T toStringRadixConvert(size_t bufLen, uint radix = 0, bool neg = false)(uint runtimeRadix = 0)
    {
        static if (neg)
            ulong div = void, mValue = cast(uint)(-value);
        else
            uint/*Unsigned!(Unqual!S)*/ div = void, mValue = cast(uint)(value);

        size_t index = bufLen;
        EEType[bufLen] buffer = void;
        char baseChar = /*letterCase == LetterCase.lower ? 'a' : */'A';
        char mod = void;

        do
        {
            static if (radix == 0)
            {
                div = cast(S)(mValue / runtimeRadix );
                mod = cast(ubyte)(mValue % runtimeRadix);
                mod += mod < 10 ? '0' : baseChar - 10;
            }
            else static if (radix > 10)
            {
                div = cast(S)(mValue / radix );
                mod = cast(ubyte)(mValue % radix);
                mod += mod < 10 ? '0' : baseChar - 10;
            }
            else
            {
                div = cast(S)(mValue / radix);
                mod = mValue % radix + '0';
            }
            buffer[--index] = cast(char)mod;
            mValue = div;
        } while (mValue);

        static if (neg)
        {
            buffer[--index] = '-';
        }
        return cast(T)buffer[index .. $].dup;
    }

    //enforce(radix >= 2 && radix <= 36, new ConvException("Radix error"));

    switch(radix)
    {
        case 10:
            if (value < 0)
                return toStringRadixConvert!(S.sizeof * 3 + 1, 10, true)();
            else
                return toStringRadixConvert!(S.sizeof * 3, 10)();
        //case 16:
        //    return toStringRadixConvert!(S.sizeof * 2, 16)();
        //case 2:
        //    return toStringRadixConvert!(S.sizeof * 8, 2)();
        //case 8:
        //    return toStringRadixConvert!(S.sizeof * 3, 8)();
        default:
            assert(0);//return toStringRadixConvert!(S.sizeof * 6)(radix);
    }
}
