// https://issues.dlang.org/show_bug.cgi?id=24184

void stage3(alias abc)(ubyte[])
{
    bool skipSpaces()
    {
        abc();
        return false;
    }
    skipSpaces;
}
ubyte[] singleThreadJsonImpl(alias xxx)(ubyte[] table)
{
    align(64) ubyte[] vector;

    ubyte[] abc() { return vector; }

    stage3!(abc)(table);

    return table;
}
ubyte[] singleThreadJsonText()
{
    bool xxx() { return true; }

    return singleThreadJsonImpl!(xxx)([]);
}
void deserializeJson() { singleThreadJsonText(); }

void main() { deserializeJson(); }
