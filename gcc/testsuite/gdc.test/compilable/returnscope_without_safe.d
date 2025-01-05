// Stack pointers are being escaped here, but without
// @safe and dip1000, it should still be allowed
// because return scope could have been inferred incorrectly,
// and it breaks existing code:
// https://issues.dlang.org/show_bug.cgi?id=23657

int* identity(return scope int* x);

auto identityAuto(int* x) => x;

int* f()
{
    int x;
    return identity(&x);
    return identityAuto(&x);
}
