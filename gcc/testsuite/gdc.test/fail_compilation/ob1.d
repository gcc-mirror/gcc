/* REQUIRED_ARGS: -preview=dip1021
TEST_OUTPUT:
---
fail_compilation/ob1.d(23): Error: variable `ob1.mars.t` has undefined state and cannot be read
---
  https://issues.dlang.org/show_bug.cgi?id=21923
*/

@live:

struct Handle
{
    private void* _handle;

    this(int n);
    ~this();
    scope void bar();
    static void fido(ref Handle);
}

void mars()
{
    auto t = Handle(10);
    t.bar();
    Handle.fido(t); // moves t to fido(), then destructor runs, causing error

    scope u = Handle(10);
}
