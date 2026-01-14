// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_function_type.

#include <meta>
using namespace std::meta;

struct cls {
  void mem_fun ();
  static void static_mem_fun ();
};
int fun (bool, char) noexcept { return 0; }

static_assert (is_function_type (type_of (^^cls::mem_fun)));
static_assert (is_function_type (type_of (^^cls::static_mem_fun)));
static_assert (is_function_type (type_of (^^::fun)));
static_assert (!is_function_type (^^void(*)(short)));
static_assert (is_function_type (remove_pointer (^^void(*)(short))));
static_assert (!is_function_type (remove_pointer (^^void(**)(short))));
static_assert (is_function_type (remove_pointer (remove_pointer (^^void(**)(short)))));
static_assert (!is_function_type (^^void(&)()));
static_assert (is_function_type (remove_reference (^^void(&)())));
using U = void(*)(void);
static_assert (!is_function_type (^^U[3]));
static_assert (is_function_type (remove_pointer (remove_extent (^^U[3]))));
static_assert (!is_function_type (remove_pointer (^^int (cls::*)())));
