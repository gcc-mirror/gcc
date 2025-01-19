/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-Wmismatched-new-delete" } */
/* PR middle-end/109224 */
/* Verify that we consider templated operator new matching with its operator
   delete.  */

#include <new>

struct foo
{
  template<typename... Args>
  void* operator new (std::size_t sz, Args&&...);
  template<typename... Args>
  void* operator new[] (std::size_t sz, Args&&...);

  void operator delete (void* x);
  void operator delete[] (void* x);

  template<typename... Args>
  void operator delete (void* x, Args&&...);
  template<typename... Args>
  void operator delete[] (void* x, Args&&...);
};

void
f ()
{
  delete (new (123, true) foo);
  delete[] (new (123, true) foo[123]);

  delete (new (123, true) foo[123]);
  // { dg-warning "Wmismatched-new-delete" "" { target *-*-* } {.-1} }
  // { dg-note "returned from" "" { target *-*-* } {.-2} }
  delete[] (new (123, true) foo);
  // { dg-warning "Wmismatched-new-delete" "" { target *-*-* } {.-1} }
  // { dg-note "returned from" "" { target *-*-* } {.-2} }

  foo::operator delete (foo::operator new (1, 123, true), 123, true);
  foo::operator delete[] (foo::operator new[] (123, 123, true), 123, true);

  foo::operator delete (foo::operator new[] (123, 123, true), 123, true);
  // { dg-warning "Wmismatched-new-delete" "" { target *-*-* } {.-1} }
  // { dg-note "returned from" "" { target *-*-* } {.-2} }
  foo::operator delete[] (foo::operator new (1, 123, true), 123, true);
  // { dg-warning "Wmismatched-new-delete" "" { target *-*-* } {.-1} }
  // { dg-note "returned from" "" { target *-*-* } {.-2} }
}
