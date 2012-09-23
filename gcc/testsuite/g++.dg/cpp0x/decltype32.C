// PR c++/50075
// { dg-options "-std=c++0x -ftemplate-depth=10" }

template <typename T>
auto make_array(const T& il) ->
decltype(make_array(il))    // { dg-error "not declared|no matching|exceeds" }
{ }

int main()
{
  int z = make_array(1);    // { dg-error "no matching" }
}
