// PR c++/50075
// { dg-do compile { target c++11 } }
// { dg-options "-ftemplate-depth=10" }

template <typename T>
auto make_array(const T& il) ->
decltype(make_array(il))    // { dg-error "not declared|no matching|depth" }
{ }

int main()
{
  int z = make_array(1);    // { dg-error "no matching" }
}

// { dg-prune-output "compilation terminated" }
