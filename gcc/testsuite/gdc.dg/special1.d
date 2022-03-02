// { dg-do compile }

struct _Complex(T) { T re; T im; }
enum __c_complex_float : _Complex!float;

bool equals(__c_complex_float[] lhs, __c_complex_float[] rhs)
{
    foreach (i; 0 .. lhs.length)
        if (lhs.ptr[i] != rhs.ptr[i])
            return false;
    return true;
}
