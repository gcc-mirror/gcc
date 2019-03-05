// { dg-do compile { target c++17 } }

template<class Element>
struct box {
    template<class E>
    constexpr box(E && e)
        noexcept(noexcept(Element(e)))
    {}
};

template<class... Ts>
struct compressed_tuple_ : box<Ts> ... {
    template<typename... Args>
    constexpr compressed_tuple_(Args &&... args)
        noexcept((noexcept(box<Ts>(args)) && ...))
      : box<Ts>(args)...
    {}
};

struct adaptor_cursor : compressed_tuple_<int*> {
    using compressed_tuple_::compressed_tuple_;
};

int main() {
    (void)noexcept(adaptor_cursor{(int*)0});
}
