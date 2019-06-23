// PR c++/69496
// { dg-do compile { target c++14 } }

constexpr int
fn_ok (int n)
{
    __extension__ int a[n] = { };
    int z = 0;

    for (unsigned i = 0; i < sizeof (a) / sizeof (int); ++i)
      z += a[i];

    return z;
}


constexpr int
fn_not_ok (int n)
{
    __extension__ int a[n] = { };
    int z = 0;

    for (unsigned i = 0; i < sizeof (a); ++i)
      z += a[i];		// { dg-error "array subscript" }

    return z;
}

constexpr int n1 = fn_ok (3);
constexpr int n2 = fn_not_ok (3); // { dg-message "in .constexpr. expansion of " }
