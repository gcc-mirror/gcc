// https://github.com/dlang/dmd/issues/21179

void bigEndianToNative(ubyte[2] a) {}

void main()
{
    ubyte[] arr;
    const ubyte[2] bytes;
    bigEndianToNative(bytes);
    auto b = cast(const ubyte[2][]) arr;
}
