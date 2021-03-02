// Verify we check return-type-requirements by passing the entire set of
// template arguments to normalization rather than first substituting into
// the constraint.  The latter approach would induce a substitution failure and
// cause the requires-expression to evaluate to false here.
// { dg-do compile { target c++20 } }

template <class, class>
concept C1 = true;

template <class T>
concept C2 = requires { { 0 } -> C1<typename T::type>; };

static_assert(C2<int>);
