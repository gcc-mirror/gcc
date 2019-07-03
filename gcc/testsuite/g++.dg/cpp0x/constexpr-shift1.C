// { dg-do compile { target c++11 } }

constexpr int
fn1 (int i, int j)
{
  return i << j; // { dg-error "is negative" }
}

constexpr int i1 = fn1 (1, -1); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn2 (int i, int j)
{
  return i << j; // { dg-error "is greater than or equal to the precision .. of the left operand" }
}

constexpr int i2 = fn2 (1, 200); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn3 (int i, int j)
{
  return i << j; // { dg-error "is negative" "" { target c++17_down } }
}

constexpr int i3 = fn3 (-1, 2); // { dg-message "in .constexpr. expansion of " "" { target c++17_down } }

constexpr int
fn4 (int i, int j)
{
  return i << j; // { dg-error "overflows" "" { target c++17_down } }
}

constexpr int i4 = fn4 (__INT_MAX__, 2); // { dg-message "in .constexpr. expansion of " "" { target c++17_down } }

constexpr int
fn5 (int i, int j)
{
  return i << j;
}

constexpr int i5 = fn5 (__INT_MAX__, 1);

constexpr int
fn6 (unsigned int i, unsigned int j)
{
  return i << j; // { dg-error "is greater than or equal to the precision .. of the left operand" }
}

constexpr int i6 = fn6 (1, -1); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn7 (int i, int j)
{
  return i >> j; // { dg-error "is negative" }
}

constexpr int i7 = fn7 (1, -1); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn8 (int i, int j)
{
  return i >> j;
}

constexpr int i8 = fn8 (-1, 1);

constexpr int
fn9 (int i, int j)
{
  return i >> j;  // { dg-error "is greater than or equal to the precision .. of the left operand" }
}

constexpr int i9 = fn9 (1, 200); // { dg-message "in .constexpr. expansion of " }
