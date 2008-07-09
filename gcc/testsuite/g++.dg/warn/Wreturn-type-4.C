/* PR c++/18313 */
/* { dg-do compile } */
/* { dg-options "-Wignored-qualifiers" } */

volatile void bar(); /* { dg-warning "type qualifiers ignored" } */

struct A
{
    const int bla(); /* { dg-warning "type qualifiers ignored" } */
    static const A getA(); /* { dg-bogus "type qualifiers" } */
};

template<typename T> const T getfoo(const T def) /* { dg-bogus "type qualifiers ignored" } */
{ return def; } 

template<typename T> class Pair
{
    public:
        T getLeft() const { return T(); }   /* { dg-bogus "type qualifiers ignored" } */
        const T getRight() const { return T(); } /* { dg-bogus "type qualifiers ignored" } */
};

template <typename T> struct S {
    const int f();                     /* { dg-warning "type qualifiers ignored" } */
    const T g();                       /* { dg-bogus "type qualifiers ignored" } */
    T h();
};

int* testtemplate()
{
    int i;

    Pair<const int> a;

    a.getLeft();
    a.getRight();

    S<bool> b;
    b.h();              /* { dg-bogus "type qualifiers ignored" } */
    b.g();              /* { dg-bogus "type qualifiers ignored" } */

    return getfoo<int*>(&i);
}
