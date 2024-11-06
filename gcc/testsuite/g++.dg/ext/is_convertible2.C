// PR c++/106784
// { dg-do compile { target c++20 } }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }
// Adapted from <https://en.cppreference.com/w/cpp/types/is_convertible>.

#include <string>
#include <string_view>

#define SA(X) static_assert((X),#X)

class E { public: template<class T> E(T&&) { } };

int main()
{
    class A {};
    class B : public A {};
    class C {};
    class D { public: operator C() { return c; }  C c; };

    SA(__is_convertible(B*, A*));
    SA(!__is_convertible(A*, B*));
    SA(__is_convertible(D, C));
    SA(!__is_convertible(B*, C*));
    SA(__is_convertible(A, E));

    using std::operator ""s, std::operator ""sv;

    auto stringify = []<typename T>(T x) {
        if constexpr (std::is_convertible_v<T, std::string> or
                      std::is_convertible_v<T, std::string_view>) {
            return x;
        } else {
            return std::to_string(x);
        }
    };

    const char* three = "three";

    SA(!__is_convertible(std::string_view, std::string));
    SA(__is_convertible(std::string, std::string_view));

    auto s1 = stringify("one"s);
    auto s2 = stringify("two"sv);
    auto s3 = stringify(three);
    auto s4 = stringify(42);
    auto s5 = stringify(42.);
}
