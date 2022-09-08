// PR c++/99209
// { dg-do compile { target c++20 } }

constexpr char f(...) = delete;
constexpr decltype(auto) f_adl(auto a) { return f(a); }

namespace A {
    constexpr char f(auto) { return 'A'; }
    template<char TemplateParam = f_adl([]{})> void g(char FunctionParam = f_adl([]{})) {
        char Local = f_adl([]{});
    }
}

namespace B {
    constexpr char f(auto) = delete;
    void call() { A::g(); }
}
