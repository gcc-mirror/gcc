// PR c++/50075
// { dg-options -std=c++0x }

template <typename T>
auto make_array(const T& il) ->	// { dg-error "not declared" }
decltype(make_array(il))
{ }

int main()
{
  int z = make_array(1);	// { dg-error "no match" }
}
