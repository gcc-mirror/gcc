// PR c++/69509
// { dg-do compile { target c++14 } }

constexpr int
fn_bad (int n)
{
  __extension__ int a [n] = { 0 };
  int z = a [0] + (n ? fn_bad (n - 1) : 0); // { dg-message "in .constexpr. expansion of " } 
  return z;
}

constexpr int
fn_ok (int n)
{
  __extension__ int a [n] = { 0 };
  int z = a [0] + (n > 1 ? fn_ok (n - 1) : 0);
  return z;
}

constexpr int i1 = fn_ok (3);
constexpr int i2 = fn_bad (3); // { dg-error "array subscript|in .constexpr. expansion of " }
