
void chkArgTypes(S, V...)()
{
    pragma(msg, S);
    static if (is(S U == __argTypes))
    {
        foreach (T; U) { pragma(msg, T); }
        static assert(U.length == V.length);
        foreach (i, T; U)
            static assert(is(V[i] == T));
    }
    else
        static assert(0);
}

void chkSingle(T,U)()
{
    struct S { T a; }
    chkArgTypes!(S, U)();
}

void chkIdentity(T)()
{
    chkSingle!(T,T)();
}

void chkPair(T,U,V)()
{
    struct S { T a; U b; }
    chkArgTypes!(S, V)();
}

version (X86_64)
{
    int main()
    {
        chkIdentity!byte();
        chkIdentity!ubyte();
        chkIdentity!short();
        chkIdentity!ushort();
        chkIdentity!int();
        chkIdentity!uint();
        chkIdentity!long();
        chkIdentity!ulong();
        chkSingle!(char,ubyte)();
        chkSingle!(wchar,ushort)();
        chkSingle!(dchar,uint)();

        chkIdentity!float();
        chkIdentity!double();
        chkIdentity!real();

        chkIdentity!(void*)();

        chkIdentity!(__vector(byte[16]))();
        chkIdentity!(__vector(ubyte[16]))();
        chkIdentity!(__vector(short[8]))();
        chkIdentity!(__vector(ushort[8]))();
        chkIdentity!(__vector(int[4]))();
        chkIdentity!(__vector(uint[4]))();
        chkIdentity!(__vector(long[2]))();
        chkIdentity!(__vector(ulong[2]))();

        chkIdentity!(__vector(float[4]))();
        chkIdentity!(__vector(double[2]))();

        chkPair!(byte,byte,short);
        chkPair!(ubyte,ubyte,short);
        chkPair!(short,short,int);
        chkPair!(int,int,long);

        chkPair!(byte,short,int);
        chkPair!(short,byte,int);

        chkPair!(int,float,long);
        chkPair!(float,int,long);
        chkPair!(byte,float,long);
        chkPair!(float,short,long);

        //struct S1 { long a; long b; }
        //chkArgTypes!(S1, long, long)();

        struct S2 { union { long a; double d; }}
        chkArgTypes!(S2, long)();

        struct S3 { union { double d; long a; }}
        chkArgTypes!(S3, long)();

        struct S4 { int a,b,c,d,e; }
        chkArgTypes!(S4)();

        struct S5 { align(1): char a; int b; }
        chkArgTypes!(S5)();

        struct S6 { align(1): int a; void* b; }
        chkArgTypes!(S5)();

        struct S7 { union { void* p; real r; }}
        chkArgTypes!(S7)();

        struct S8 { union { real r; void* p; }}
        chkArgTypes!(S8)();

        return 0;
    }
}
else
{
    int main()
    {
        return 0;
    }
}
