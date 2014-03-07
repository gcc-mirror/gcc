// { dg-do compile { target c++11 } }

// alias template of a partial specialization

template<class T, class U, class W> struct S0 {};
template<class T, class U> struct S0<T, U, char> {};
template<class T> using AS0 = S0<T, int, char>;
void foo(S0<bool, int, char>);

AS0<bool> a; // OK

void
f()
{
    foo(a); //OK
}

// alias template of an explicit specialization of a member template

template<class T>
struct S1 {
    template<class U>
    struct M {};
};
template<class T> using AM = S1<int>::M<T>;
void bar(S1<int>::M<bool>);

AM<bool> b; //OK.

void
g()
{
    bar(b); //OK
}
