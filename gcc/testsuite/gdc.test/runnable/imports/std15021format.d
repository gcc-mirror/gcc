module imports.std15021format;

T enforceEx(T)(T value, lazy string msg = "")
{
    if (!value) throw new Exception(msg);
    return value;
}

void enforceValidFormatSpec(T, Char)(int spec)
{
    enforceEx(spec == 's');
}
