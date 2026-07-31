// PR c++/126423
// { dg-do compile { target c++14 } }
// { dg-options "" }

auto
foo ()
{
  template for (auto x : {})	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    return x;
}
