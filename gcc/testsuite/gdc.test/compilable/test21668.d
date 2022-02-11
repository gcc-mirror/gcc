// https://issues.dlang.org/show_bug.cgi?id=21668

struct Opaque;

void byPtr(Opaque*) {}
void byRef(ref Opaque) {} // Fails
void bySlice(Opaque[]) {}
