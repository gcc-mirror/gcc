// https://issues.dlang.org/show_bug.cgi?id=23778


enum __c_long_double : double;

alias __c_long_double c_long_double;

struct _Complex
{
    c_long_double re;
    c_long_double im;
}

version (all) // bug to test
{
    enum __c_complex_real   : _Complex;
    alias c_complex_real = __c_complex_real;
}
else // works
    enum c_complex_real   : _Complex;

c_complex_real toNative2(real re, real im)
{
    return c_complex_real(re, im);
}

void main()
{
    c_complex_real n = toNative2(123, 456);
    assert(123 == n.re && 456 == n.im);
}
