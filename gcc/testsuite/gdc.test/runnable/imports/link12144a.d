module imports.link12144a;
struct S1 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S2 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S3 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S4 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S5 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S6 { bool opEquals(T : typeof(this))(T) { return false; } ~this(){} }
struct S7 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S8 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S9 { bool opEquals(T : typeof(this))(T) { return false; } }

struct S10 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S11 { bool opEquals(T : typeof(this))(T) const { return false; }
             int opCmp(T : typeof(this))(T) const { return 0; }
             size_t toHash() const nothrow @safe { return 0; } }
struct S12 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S13 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S14 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S15 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S16 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S17 { bool opEquals(T : typeof(this))(T) { return false; } }
struct S18 { bool opEquals(T : typeof(this))(T) { return false; } }

void fun()()
{
    { auto a = new S1[1]; }
    { auto p = new S2(); }
    { alias P = S3*; auto p = new P; }
    { S4[int] aa; auto b = (aa == aa); }
    { S5[] a; a.length = 10; }
    { S6[] a; delete a; }
    { S7[] a = []; }
    { S8[] a = [S8.init]; }
    { S9[int] aa = [1:S9.init]; }

    { auto ti = typeid(S10[int]); }
    { auto ti = typeid(int[S11]); }
    { auto ti = typeid(S12[]); }
    { auto ti = typeid(S13*); }
    { auto ti = typeid(S14[3]); }
    { auto ti = typeid(S15 function()); }
    { auto ti = typeid(S16 delegate()); }
    { auto ti = typeid(void function(S17)); }   // TypeInfo_Function doesn't have parameter types
    { auto ti = typeid(void delegate(S18)); }   // ditto
}

struct B12146
{
    bool opCmp(ubyte val) { return false; }
}
