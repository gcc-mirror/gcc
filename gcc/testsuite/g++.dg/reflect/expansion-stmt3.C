// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>
#include <string_view>

int volatile sink = 0;
void use(auto&& x) {
    sink += sizeof(x);
}

template <typename T>
void reflect_struct(T const& obj) {
    static constexpr std::meta::info mems[] = {^^T::name, ^^T::age, ^^T::active};

    template for (constexpr std::meta::info field : mems) {
        use(std::meta::identifier_of(field));
        use(obj.[:field:]);
    }
}

template <int>
struct User {
    std::string_view name;
    int age;
    bool active;
};

int main() {
    reflect_struct(User<0>{.name = "Alice", .age = 30, .active = true});
}
