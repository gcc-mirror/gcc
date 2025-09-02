// PR c++/121583
// { dg-do compile { target c++14 } }
// { dg-options "" }

auto
foo ()
{
  template for (int i : { 0 })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    return i;
}

auto
bar ()
{
  template for (auto i : { 0 })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    return i;
}
