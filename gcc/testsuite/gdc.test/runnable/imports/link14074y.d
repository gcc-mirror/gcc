module imports.link14074y;

void encode(R, E)(R sink, E value)
{
    encodeInt(sink);
    encodeArray(sink, value);

    static assert(false);
}

void encodeInt(R)(R sink)
{
    encodeLongType(sink);
}

void encodeLongType(R)(R sink)
{
    import imports.link14074x;
    put(sink);
}

void encodeArray(R, A)(R sink, A)
{
    encodeArrayHead(sink,
        __traits(compiles, { encode(cast(ubyte[])null, A.tupleof[0].init); }));
}

void encodeArrayHead(R)(R sink, ulong arrayLength)
{
    encodeLongType(sink);
}
