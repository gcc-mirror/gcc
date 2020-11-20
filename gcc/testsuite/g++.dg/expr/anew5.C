// PR c++/97523
// { dg-do compile }
// We were turning the () into {} which made it seem like
// aggregate-initialization (we are dealing with arrays here), which
// performs copy-initialization, which only accepts converting constructors.

struct T {
  explicit T();
  T(int);
};

void
fn (int n)
{
  new T[1]();
  new T[2]();
  new T[3]();
  new T[n]();
#if __cpp_aggregate_paren_init
  new T[]();
  new T[2](1, 2);
  // T[2] is initialized via copy-initialization, so we can't call
  // explicit T().
  new T[3](1, 2); // { dg-error "explicit constructor" "" { target c++20 } }
#endif
}
