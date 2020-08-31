// https://bugzilla.gdcproject.org/show_bug.cgi?id=187
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

align(1) struct S187b
{
    align(1)
    {
        uint unpaddedA;
        ushort unpaddedB;
    }
}

struct S187a
{
    S187b[3] unpaddedArray;
    ubyte wontInitialize = ubyte.init;
}

struct S187
{
    S187a interesting;
}


void prepareStack()
{
    byte[255] stackGarbage;
    foreach(i, ref b; stackGarbage)
    {
        b  = cast(byte)(-i);
    }
}

void main()
{
    prepareStack();
    auto a = S187(S187a());
    assert(a.interesting.wontInitialize == 0);
}
