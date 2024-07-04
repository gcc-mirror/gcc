// PR c++/109715
// { dg-do compile { target c++11 } }

template<class T>
[[gnu::abi_tag("foo")]] void fun() { }

template void fun<int>();

#if __cpp_variable_templates
template<class T>
[[gnu::abi_tag("foo")]] int var = 0;

template int var<int>;
#endif

// { dg-final { scan-assembler "_Z3funB3fooIiEvv" } }
// { dg-final { scan-assembler "_Z3varB3fooIiE" { target c++14 } } }
