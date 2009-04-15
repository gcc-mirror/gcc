/* { dg-do compile } */

class A;
class B { };
extern const double NaN;
B foo(A* exec, double d);
inline B baz(A* a)     {
    return foo(a, NaN);
}
B bar(A* a) {
    return baz(a);
}
extern const double NaN = (__builtin_nanf(""));

