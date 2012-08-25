// PR c++/50075
// { dg-options -std=c++0x }

template <typename T>
auto make_array(const T& il) ->
decltype(make_array(il))	// { dg-error "not declared" }
{ }

int main()
{
  int z = make_array(1);	// { dg-error "no match" }
}
