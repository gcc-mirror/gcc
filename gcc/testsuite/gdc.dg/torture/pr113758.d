// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
struct S113758
{
    int field;
    ~this() { field = 0; }
}

void main()
{
    auto var = S113758(1);
    f113758d(var);
    assert(var.field == 1);
    f113758cxx(var);
    assert(var.field == 1);
}

extern (D)   void f113758d(S113758 arg) { }
extern (C++) void f113758cxx(S113758 arg) { }
