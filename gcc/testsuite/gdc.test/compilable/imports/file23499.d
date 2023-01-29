
///---- file.d
struct TempCStringBuffer(To )
{
    ~this()
    {
    }
    To* _ptr;
}

auto tempCString(To, From)(From)
{
    return TempCStringBuffer!To();
}

bool exists(R)(R name)
{
    return name.tempCString!wchar._ptr != null;
}
