// PR c++/68918
// { dg-do compile { target c++11 } }

struct foo {
    static void a() {}

    auto b() const -> decltype( this->a() )
    {}

    template<typename X>
    auto c() -> decltype( this->a() )
    {}

    template<typename X>
    auto d() const -> decltype( a() )
    {}

    template<typename X>
    auto e() const -> decltype( this->a() )
    {}
};

int main()
{
}
