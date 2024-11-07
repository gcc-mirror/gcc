// PR c++/90321
// { dg-do compile { target c++17 } }

template<class F> struct hack : F { };
template<class F> hack(F) -> hack<F>;

int main()
{
    auto f = [x = 1, y = 2]() { };
    auto [a, b] = hack { f };  // { dg-error "cannot decompose lambda closure type" }
    return b;
}
