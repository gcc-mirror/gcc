// https://issues.dlang.org/show_bug.cgi?id=20130

void main() {
    auto a1 = cast(char[])  "12345678";
    auto a2 = cast(wchar[]) "12345678"; // Or cast(string|wstring|dstring).
    auto a3 = cast(dchar[]) "12345678"; // Encoding conversion.
    assert(a1.length == 8);
    assert(a2.length == 8);
    assert(a3.length == 8);

    auto b1 = cast(char[])  "12345678"c;
    auto b2 = cast(wchar[]) "12345678"c;
    auto b3 = cast(dchar[]) "12345678"c;
    assert(b1.length == 8);
    assert(b2.length == 4);
    assert(b3.length == 2);

    auto c1 = cast(char[])  "12345678"w;
    auto c2 = cast(wchar[]) "12345678"w;
    auto c3 = cast(dchar[]) "12345678"w;
    assert(c1.length == 16);
    assert(c2.length == 8);
    assert(c3.length == 4);

    auto d1 = cast(char[])  "12345678"d;
    auto d2 = cast(wchar[]) "12345678"d;
    auto d3 = cast(dchar[]) "12345678"d;
    assert(d1.length == 32);
    assert(d2.length == 16);
    assert(d3.length == 8);

    auto a = cast(uint[]) "12345678";
    auto b = cast(uint[]) "12345678"d;
    auto c = cast(uint[]) "12345678"w;
    auto d = cast(const char[5][]) "12345";
    auto e = cast(const wchar[2][]) "12345678";
    immutable f = cast(immutable(uint)[]) "123456789012";
    assert(a.length == 2);
    assert(b.length == 8);
    assert(c.length == 4);
    assert(d.length == 1);
    assert(e.length == 2);
    assert(f.length == 3);
}
