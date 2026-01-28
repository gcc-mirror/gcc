// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
struct S123422
{
    S123422* ptr;
    this(int) { ptr = &this; }
    this(ref inout S123422) { ptr = &this; }
}

struct V123422
{
    S123422 s;
    this(int) { s = S123422(1); }
}

S123422 foo()
{
    return V123422(1).s;
}

void main()
{
    S123422 s = foo();
    assert(&s == s.ptr);
}
