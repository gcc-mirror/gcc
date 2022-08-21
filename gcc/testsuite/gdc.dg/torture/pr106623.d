// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=106623
// { dg-do compile }
private struct _Complex(T) { T re; T im; }
enum __c_complex_double : _Complex!double;

pragma(inline, true)
ulong hashOf()(scope const double val)
{
    return *cast(ulong*)&val;
}

pragma(inline, true)
ulong hashOf()(scope const _Complex!double val, ulong seed = 0)
{
    return hashOf(val.re) + hashOf(val.im);
}

pragma(inline, true)
ulong hashOf()(__c_complex_double val, ulong seed = 0)
{
    return hashOf(cast(_Complex!double) val, seed);
}

ulong test106623()
{
    __c_complex_double val;
    return hashOf(val);
}
