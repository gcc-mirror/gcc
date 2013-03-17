// PR c++/54946
// { dg-do compile { target c++11 } }

template<const char*s>    static void testfunc();
constexpr struct testtype { const char* str; } test = { "abc"} ;
void (*functionpointer)() = testfunc<(const char*) test.str>; // { dg-error "" }
