// { dg-do compile { target c++11 } }

/* <memory>.  */

template<class T>
void test_make_shared ()
{
  auto p = std::make_shared<T>(); // { dg-error "'make_shared' is not a member of 'std'" }
  // { dg-message "'#include <memory>'" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before '>' token" "" { target *-*-* } .-2 }
  // { dg-error "expected primary-expression before '\\)' token" "" { target *-*-* } .-3 }
}

std::shared_ptr<int> test_shared_ptr; // { dg-error "'shared_ptr' in namespace 'std' does not name a template type" }
// { dg-message "'#include <memory>'" "" { target *-*-* } .-1 }

std::unique_ptr<int> test_unique_ptr; // { dg-error "'unique_ptr' in namespace 'std' does not name a template type" }
// { dg-message "'#include <memory>'" "" { target *-*-* } .-1 }

std::weak_ptr<int> test_weak_ptr; // { dg-error "'weak_ptr' in namespace 'std' does not name a template type" }
// { dg-message "'#include <memory>'" "" { target *-*-* } .-1 }

/* <tuple>.  */

void test_make_tuple (int i, int j, int k)
{
  auto t = std::make_tuple (i, j, k); // { dg-error "'make_tuple' is not a member of 'std'" }
  // { dg-message "'#include <tuple>'" "" { target *-*-* } .-1 }
}

/* <utility>.  */

template<class T>
void test_forward(T&& arg) 
{
  std::forward<T>(arg); // { dg-error "'forward' is not a member of 'std'" }
  // { dg-message "'#include <utility>'" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before '>' token" "" { target *-*-* } .-2 }
}

void test_make_pair (int i, int j)
{
  auto p = std::make_pair (i, j); // { dg-error "'make_pair' is not a member of 'std'" }
  // { dg-message "'#include <utility>'" "" { target *-*-* } .-1 }
}

template<class T>
void test_move(T&& arg) 
{
  std::move<T>(arg); // { dg-error "'move' is not a member of 'std'" }
  // { dg-message "'#include <utility>'" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before '>' token" "" { target *-*-* } .-2 }
}

void test_array ()
{
  std::array a; // { dg-error ".array. is not a member of .std." }
  // { dg-message ".std::array. is defined in header .<array>.; did you forget to .#include <array>.?" "" { target *-*-* } .-1 }
}

void test_tuple ()
{
  std::tuple<int,float> p; // { dg-error ".tuple. is not a member of .std." }
  // { dg-message ".std::tuple. is defined in header .<tuple>.; did you forget to .#include <tuple>.?" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before .int." "" { target *-*-* } .-2 }
}
