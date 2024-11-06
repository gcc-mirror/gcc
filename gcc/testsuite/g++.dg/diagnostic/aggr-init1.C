// PR c++/116634
// { dg-do compile { target c++11 } }
// { dg-additional-options -fpermissive }

namespace std {
  using size_t = decltype(sizeof(42));
}

class ConstString final {
public:
    constexpr ConstString() noexcept: buf(), len(0) {}
    template<int N>
    constexpr ConstString(const char (&a)[N]): buf(a), len(N - 1) {}
    constexpr ConstString(const ConstString &c1): buf(c1.buf), len(static_cast<int>(c1.len)) {}

private:
    const char* buf;
    int len;
};

template<int N>
struct Any final {
    constexpr
    Any(ConstString (&&_vec)[N]) noexcept: vec(_vec){} // { dg-warning "array" }

    ConstString vec[N];
};

template<int... N1>
constexpr static
auto Any_of(const char (&...c1)[N1]) -> Any<static_cast<int>(sizeof...(N1))> {
  return {{ConstString(c1)...}};
}

int main() {
  constexpr static const auto aa1 = Any_of("abcd", "def"); // { dg-message {"abcd", "def"} }
}
