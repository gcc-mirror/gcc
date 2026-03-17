// PR c++/123662
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct arg {
    std::meta::info name;
    bool none = true;
};

template<size_t...Is>
void test1() {
    constexpr auto py_arg_data = std::array{arg{std::meta::reflect_constant_string("_")}};
    constexpr auto short_py_arg_data = [: std::meta::reflect_constant_array(py_arg_data) :];
}
