// { dg-additional-options "-fmodules-ts -Wno-pedantic" }

module;
       
# 6 "std" 1
template<typename T>
class basic_string_view
{
public:
  basic_string_view(const char* __str) noexcept;
};

# 14 "" 2
export module hello;
// { dg-module-cmi hello }
export void greeter (basic_string_view<char> const &name)
{

}
