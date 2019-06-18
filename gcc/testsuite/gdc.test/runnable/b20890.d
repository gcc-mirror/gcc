module b20890;
// https://issues.dlang.org/show_bug.cgi?id=20890

struct S0 { }
void format0(string spec, S0[1] s)
{
    assert (spec    == "lengthy");
}
struct S1 { ubyte m = 42; }
void format1(string spec, S1[1] s)
{
    assert (spec    == "lengthy");
    assert (s[0].m  == 42);
}
struct S2 { ushort m = 42; }
void format2(string spec, S2[1] s)
{
    assert (spec    == "lengthy");
    assert (s[0].m  == 42);
}
struct S4 { uint m = 42; }
void format4(string spec, S4[1] s)
{
    assert (spec    == "lengthy");
    assert (s[0].m  == 42);
}
struct S8 { ulong m = 42; }
void format8(string spec, S8[1] s)
{
    assert (spec    == "lengthy");
    assert (s[0].m  == 42);
}
struct S42 { ubyte[42] m = [42]; }
void format42(string spec, S42[1] s)
{
    assert (spec        == "lengthy");
    assert (s[0].m[0]   == 42);
}

void main()
{
    { S0[1] s; format0("lengthy", s); }
    { S1[1] s; format1("lengthy", s); }
    { S2[1] s; format2("lengthy", s); }
    { S4[1] s; format4("lengthy", s); }
    { S8[1] s; format8("lengthy", s); }
    {S42[1] s;format42("lengthy", s); }
}
