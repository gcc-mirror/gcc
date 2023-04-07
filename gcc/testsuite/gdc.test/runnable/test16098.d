
// https://issues.dlang.org/show_bug.cgi?id=16098

void main() {
    byte a;
    align(128) byte b;
    assert((cast(size_t) &b) % 128 == 0);

    byte foo() { return b; }
    dg = &foo;
    assert(dg() == false);
}

__gshared byte delegate() dg;
