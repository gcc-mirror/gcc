// https://issues.dlang.org/show_bug.cgi?id=20714
// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail20714.d(19): Deprecation: `struct Adder` implicitly-generated postblit hides copy constructor.
fail_compilation/fail20714.d(19):        The field postblit will have priority over the copy constructor.
fail_compilation/fail20714.d(19):        To change this, the postblit should be disabled for `struct Adder`
---
*/


struct Blitter
{
    int payload;
    this(this){}
}

struct Adder
{
    Blitter blitter;
    this(int payload) {this.blitter.payload = payload;}
    this(ref Adder rhs) {this.blitter.payload = rhs.blitter.payload + 1;}
}

void main()
{
    Adder piece1 = 1;
    auto piece2 = piece1;

    assert(piece2.blitter.payload == 2);
}
