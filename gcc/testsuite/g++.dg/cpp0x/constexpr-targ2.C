// PR c++/65498
// { dg-do compile { target c++11 } }

template <typename, typename>
struct is_same
{
  enum { value = false };
  constexpr bool operator()() const noexcept { return value; }
};

template <typename T>
struct is_same<T, T>
{
  enum { value = true };
  constexpr bool operator()() const noexcept { return value; }
};

template <bool, typename = void>
struct enable_if { };

template <typename T>
struct enable_if<true, T> { typedef T type; };

struct A;

template <typename, typename = void>
struct F { };

template <typename X>
struct F<X, typename enable_if<is_same<X, A>{}()>::type> {
    template <typename MakeDependent>
    F(MakeDependent) {
        auto ICE_HERE = __func__;
        (void)ICE_HERE; // avoid -Wunused-variable
    }
};

int main() {
    F<A>{1};
}
