// PR c++/103403
// { dg-do compile { target c++14 } }

template<typename T>
auto constexpr RtoL1(T&& r) -> decltype(auto) {
    return (r);
};
int main() {
    int t;
    int x{3};
    decltype (RtoL1(x+0)) y = t; // { dg-error "cannot bind rvalue reference" "" { target c++23 } }
}
