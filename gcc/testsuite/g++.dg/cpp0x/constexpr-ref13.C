// P2280R4 - Using unknown pointers and references in constant expressions
// PR c++/106650
// { dg-do compile { target c++11 } }

using size_t = decltype(sizeof(42));

template <typename T, size_t N>
constexpr auto array_size(T (&)[N]) -> size_t {
    return N;
}

extern int (&r)[42];
constexpr int i = array_size (r);

void check(int const (&param)[3]) {
    int local[] = {1, 2, 3};
    constexpr auto s0 = array_size(local);
    constexpr auto s1 = array_size(param);
}

template <typename T, size_t N>
constexpr size_t array_size_ptr(T (*)[N]) {
    return N;
}

void check_ptr(int const (*param)[3]) {
    constexpr auto s2 = array_size_ptr(param); // { dg-error "not a constant" }
}

struct A
{
   constexpr int f() { return 42; }
   void g() { constexpr int i = f(); }
   void g2() { constexpr int i = this->f(); }
};

struct B {
  constexpr static bool b = false;
  void g() noexcept(b) { }
  void g2() noexcept(this->b) { }
};
