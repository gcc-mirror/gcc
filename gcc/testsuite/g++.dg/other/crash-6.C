// Origin: PR c++/42634
// { dg-options "-g -std=c++0x" }
// { dg-do compile }

template<typename T> T declval();

template<typename T, typename... Args> struct is_constructible {
    template<typename T1, typename... Args1> static decltype(T1(declval<Args1>()...), char()) test();
    static const bool value = sizeof(test<T, Args...>()) == 1;
};
template<bool> struct enable_if {
        typedef void type;
};
template<class T1, class T2> struct pair {
    template<class U2,
             class = typename enable_if<is_constructible<T2,U2&&>::value>::type
             >
    pair(const T1&, U2&&) { }
};
struct string {
  string() : p(0) {}
  char* p;
};

struct Foo {
  string s;
  int i;
};

void f()
{
  pair<int, Foo>(1, Foo());
}

