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
static assert(C2.alignof == size_t.sizeof);
static assert(__traits(classInstanceSize, C2) == payloadOffset + 12);
