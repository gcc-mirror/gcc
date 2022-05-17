struct S1 // overall alignment: max(1, 1) = 1
{
    byte[5] bytes;
    struct // overall alignment: max(1, 1) = 1
    {
        byte byte1;
        align(1) int int1;
    }
}

static assert(S1.int1.offsetof == 6);
static assert(S1.alignof == 1);
static assert(S1.sizeof == 10);

class C2 // overall alignment: max(vtbl.alignof, monitor.alignof, 1, 2)
{
    byte[5] bytes;
    struct // overall alignment: max(1, 2) = 2
    {
        byte byte1;
        align(2) int int1;
    }
}

enum payloadOffset = C2.bytes.offsetof;
static assert(C2.int1.offsetof == payloadOffset + 8);
static assert(__traits(classInstanceAlignment, C2) == size_t.sizeof);
static assert(__traits(classInstanceSize, C2) == payloadOffset + 12); // no tail padding

align(8) struct PaddedStruct
{
    bool flag;
    align(2) S1 s1;
}

static assert(PaddedStruct.s1.offsetof == 2);
static assert(PaddedStruct.alignof == 8);
static assert(PaddedStruct.sizeof == 16);

class AlignedPayloadClass
{
    align(64) int field;
}

static assert(AlignedPayloadClass.field.offsetof == 64); // vtbl, monitor, alignment padding
static assert(__traits(classInstanceAlignment, AlignedPayloadClass) == 64);
static assert(__traits(classInstanceSize, AlignedPayloadClass) == 68);

align(1) struct UglyStruct
{
    bool flag;
    int i;
    ubyte u;
}

static assert(UglyStruct.i.offsetof == 4);
static assert(UglyStruct.alignof == 1);
static assert(UglyStruct.sizeof == 9);

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=19914
// https://issues.dlang.org/show_bug.cgi?id=19915

class TemplatedClass(T)
{
    align T field;
}

mixin TemplatedClass!(string);
alias TCint = TemplatedClass!(int);
