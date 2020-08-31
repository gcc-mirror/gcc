// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96156
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S96156
{
    __gshared void* ptr;
    int x;

    this(int x) { ptr = &this; this.x = x; }
    @disable this(this);
}

auto f96156()
{
    return g96156();
}

auto g96156()
{
    return h96156();
}

auto h96156()
{
    return S96156(100);
}

void main()
{
    auto s = f96156();
    assert(&s == S96156.ptr);
}
