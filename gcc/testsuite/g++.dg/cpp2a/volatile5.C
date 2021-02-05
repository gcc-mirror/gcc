// PR c++/98947
// { dg-do compile }

volatile int x, y, z;

void
f (bool b)
{
  (b ? x : y) = 1;
  (b ? x : y) += 1; // { dg-warning "compound assignment" "" { target c++20 } }
  z = (b ? x : y) = 1; // { dg-warning "using value of simple assignment" "" { target c++20 } }
  ((z = 2) ? x : y) = 1; // { dg-warning "using value of simple assignment" "" { target c++20 } }
  (b ? (x = 2) : y) = 1; // { dg-warning "using value of simple assignment" "" { target c++20 } }
  (b ? x : (y = 5)) = 1; // { dg-warning "using value of simple assignment" "" { target c++20 } }
}
