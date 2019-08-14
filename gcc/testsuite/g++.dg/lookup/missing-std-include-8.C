/* Verify that we don't offer #include suggestions for things that
   aren't yet available due to the C++ dialect in use.  */
// { dg-do compile { target c++98_only } }

#include <memory>

template<class T>
void test_make_shared ()
{
  std::make_shared<T>(); // { dg-error "'make_shared' is not a member of 'std'" }
  // { dg-message "'std::make_shared' is only available from C\\+\\+11 onwards" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before '>' token" "" { target *-*-* } .-2 }
  // { dg-error "expected primary-expression before '\\)' token" "" { target *-*-* } .-3 }
}

template<class T>
void test_make_unique ()
{
  std::make_unique<T>(); // { dg-error "'make_unique' is not a member of 'std'" }
  // { dg-message "'std::make_unique' is only available from C\\+\\+14 onwards" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before '>' token" "" { target *-*-* } .-2 }
  // { dg-error "expected primary-expression before '\\)' token" "" { target *-*-* } .-3 }
}

void test_array ()
{
  std::array a; // { dg-error "'array' is not a member of 'std'" }
  // { dg-message "'std::array' is only available from C\\+\\+11 onwards" "" { target *-*-* } .-1 }
}

void test_tuple ()
{
  std::tuple<int,float> p; // { dg-error "'tuple' is not a member of 'std'" }
  // { dg-message "'std::tuple' is only available from C\\+\\+11 onwards" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before 'int'" "" { target *-*-* } .-2 }
}

/* Since C++14.  */
std::shared_timed_mutex m; // { dg-error "'shared_timed_mutex' in namespace 'std' does not name a type" }
// { dg-message "'std::shared_timed_mutex' is only available from C\\+\\+14 onwards" "" { target *-*-* } .-1 }

/* Since C++17: */
std::string_view sv; // { dg-error "'string_view' in namespace 'std' does not name a type" }
// { dg-message "'std::string_view' is only available from C\\+\\+17 onwards" "" { target *-*-* } .-1 }

/* Verify interaction with "using namespace std;".  */
using namespace std;
void test_via_using_directive ()
{
  shared_ptr<int> p; // { dg-error "'shared_ptr' was not declared in this scope" }
  // { dg-message "'std::shared_ptr' is only available from C\\+\\+11 onwards" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before 'int'" "" { target *-*-* } .-2 }
}
