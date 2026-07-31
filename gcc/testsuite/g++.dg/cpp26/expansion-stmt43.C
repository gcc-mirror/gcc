// PR c++/126420
// { dg-do compile { target c++14 } }
// { dg-options "" }

auto
foo ()
{
  template for (auto x : {})	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    return 42;
  return 42L;
}
