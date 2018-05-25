// P0614R1
// { dg-do compile { target c++11 } }
// { dg-options "" }

void
fn1 ()
{
  int a[] = { 1, 2, 3, 4, 5 };

  for (int i = 0; auto x : a) // { dg-warning "range-based .for. loops with initializer only available with" "" { target c++17_down } }
    ++i;

  int i;
  for (i = 0; auto x : a) // { dg-warning "range-based .for. loops with initializer only available with" "" { target c++17_down } }
    ++i;
}
