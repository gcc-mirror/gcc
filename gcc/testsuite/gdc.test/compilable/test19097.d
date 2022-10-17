/* REQUIRED_ARGS: -preview=dip1000
 */

// Related to: https://github.com/dlang/dmd/pull/8504

@safe:

void betty()(ref int* r, return scope int* p)
{
    r = p; // infer `scope` for r
}

void boop()(ref int* r, scope int* p)
{
    r = p; // infer `scope` for r, `return` for p
}

void foo(scope int* pf)
{
    scope int* rf;
    betty(rf, pf);
    boop(rf, pf);
}

// https://issues.dlang.org/show_bug.cgi?id=22801
struct Wrapper
{
    int* ptr;

    this(return ref int var) @safe
    {
        this.ptr = &var;
    }
}

void main() @safe
{
    int i;
    auto w = Wrapper(i);
    auto wt = WrapperT!()(i);
}

void assign(ref scope int* x, return ref int y) @safe
{
    x = &y;
}

// https://issues.dlang.org/show_bug.cgi?id=22967
// inference of `return ref` when assigned to first parameter
struct WrapperT()
{
    int* ptr;

    this(ref int var) @safe
    {
        this.ptr = &var;
    }

    static void assignInferred(ref scope int* xi, ref int yi) @safe
    {
        xi = &yi;
    }
}
