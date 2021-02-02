
constexpr void duration_cast ()
{
  // the constexpr's body's clone merely duplicates the TYPE_DECL, it
  // doesn't create a kosher typedef
  typedef int __to_rep;
}
