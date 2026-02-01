// PR c++/123614
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

consteval auto Name(const std::meta::info meta){
    same_type<decltype(meta), const std::meta::info>();
    return std::meta::display_string_of(meta);
}

int main() {
  auto sv = std::define_static_string(Name(^^int));
}
