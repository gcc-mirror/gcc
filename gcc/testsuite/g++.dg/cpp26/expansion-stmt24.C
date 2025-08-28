// PR c++/121583
// { dg-do compile { target c++14 } }
// { dg-options "" }

auto
foo ()
{
  template for (auto i : { 0, 1, 2LL })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    return i;					// { dg-error "inconsistent deduction for auto return type: 'int' and then 'long long int'" }
}
