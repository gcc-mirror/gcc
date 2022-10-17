// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=105004
// { dg-do compile }

private struct _Complex(T)
{
    T re;
    T im;
}
enum __c_complex_float  : _Complex!float;

__c_complex_float pr105004(float re, float im)
{
    return typeof(return)(re, im);
}
