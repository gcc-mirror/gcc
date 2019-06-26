// PR c++/89480
// { dg-do compile { target c++11 } }

template <typename Foo, Foo Part>
struct TSelect {};

enum What {
    The
};

template <typename Foo>
struct AnotherOneSelector {
    static constexpr Foo Id = Foo::The;
};

template <typename Foo, typename SelectPartType>
struct THelper;

template <typename Foo>
struct THelper<Foo, TSelect<Foo, Foo{AnotherOneSelector<Foo>::Id}>> {};

int main() {
    THelper<What, TSelect<What, What::The>> t;
}
