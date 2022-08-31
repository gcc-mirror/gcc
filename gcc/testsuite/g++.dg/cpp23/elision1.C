// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++23 } }
// Tests from P2266R1.

namespace std {
  template<typename _Tp>
    struct remove_reference
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp   type; };

  template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
}

template<typename T, typename U>
struct same_type { static const bool value = false; };

template<typename T>
struct same_type<T, T> { static const bool value = true; };

struct Widget {
    Widget(Widget&&);
};

struct RRefTaker {
    RRefTaker(Widget&&);
};

struct Mutt {
    operator int*() &&;
};

struct Jeff {
    operator int&() &&;
};

struct Ella {
    operator int() &&;
};

Widget one(Widget w) {
    return w;  // OK since C++11
}

RRefTaker two(Widget w) {
    return w;  // OK since C++11 + CWG1579
}

RRefTaker three(Widget&& w) {
    return w;  // OK since C++20 because P0527
}

// Tests that implicit move applies even to functions that return references.
Widget&& four(Widget&& w) {
    return w;  // OK since C++23
}

// ... or pointers.
int* five(Mutt x) {
    return x;  // OK since C++20 because P1155
}

int& six(Jeff x) {
    return x;
}

int test_ella(Ella e) {
  return e;
}

template<class T>
T&& seven(T&& x) { return x; }

void test_seven(Widget w) {
    Widget& r = seven(w);
    Widget&& rr = seven(std::move(w));
}

Widget val();
Widget& lref();
Widget&& rref();

decltype(auto) eight() {
    decltype(auto) x = val();  // OK, x is Widget
    return x;  // OK, return type is Widget, we get copy elision
}

decltype(auto) nine() {
    decltype(auto) x = lref();  // OK, x is Widget&
    return x;  // OK, return type is Widget&
}

decltype(auto) ten() {
  decltype(auto) x = rref();  // OK, x is Widget&&
  // This was an error: return type is Widget&&, cannot bind to x.
  // But in C++23, x is treated as an rvalue.
  return x;
}

// Now returns Widget&&, not Widget&.
// This is from $ 3.2.1. Interaction with decltype and decltype(auto).
decltype(auto) eleven(Widget&& x) {
    return (x);
}
static_assert(same_type<decltype(eleven), Widget&& (Widget&&)>::value);
