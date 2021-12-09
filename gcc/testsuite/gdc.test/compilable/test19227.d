// https://issues.dlang.org/show_bug.cgi?id=19227
/* TEST_OUTPUT:
---
compilable/test19227.d(18): Deprecation: use of complex type `cfloat` is deprecated, use `std.complex.Complex!(float)` instead
Deprecation: use of complex type `const(cfloat)` is deprecated, use `std.complex.Complex!(float)` instead
Deprecation: use of complex type `const(cfloat)` is deprecated, use `std.complex.Complex!(float)` instead
Deprecation: use of complex type `const(cfloat)` is deprecated, use `std.complex.Complex!(float)` instead
---
*/

struct S
{
    float f;
}

struct T
{
    cfloat cf;
}

void main()
{
    static assert(S.init is S.init);
    static assert(S.init != S.init);

    static assert(T.init is T.init);
    static assert(T.init != T.init);
}

