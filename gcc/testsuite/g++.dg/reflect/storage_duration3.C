// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_{static,thread,automatic}_storage_duration.

#include <meta>

using namespace std::meta;

struct A { const int ci = 0; int nci; };
struct B : A { mutable int i; };

B arr[2];
const A &r = arr[1];
static_assert (has_static_storage_duration (^^arr));
static_assert (!has_thread_storage_duration (^^arr));
static_assert (!has_automatic_storage_duration (^^arr));
static_assert (has_static_storage_duration (^^r));
static_assert (!has_thread_storage_duration (^^r));
static_assert (!has_automatic_storage_duration (^^r));
