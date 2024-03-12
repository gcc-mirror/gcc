// REQUIRED_ARGS: -preview=dip1021 -o-
// https://issues.dlang.org/show_bug.cgi?id=23986
// dip1021 asserts on `typeof(null)` parameter
@safe:

void f(typeof(null) obj, int* x) {}

void g()
{
    f(null, null);
}
