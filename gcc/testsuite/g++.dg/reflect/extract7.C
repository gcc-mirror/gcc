// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::extract.

#include <meta>

using namespace std::meta;

void fn();
static_assert(annotations_of(^^fn).size() == 0);
[[=1, =2]] void fn();
static_assert(annotations_of(^^fn).size() == 2);
[[=3]] void fn();
static_assert(annotations_of(^^fn).size() == 3);
[[=4, =5]] void fn();
static_assert(annotations_of(^^fn).size() == 5);
[[=6]] void fn();
static_assert(annotations_of(^^fn).size() == 6);
void fn();
static_assert(annotations_of(^^fn).size() == 6);

static_assert(extract<int>(annotations_of(^^fn)[0]) == 1);
static_assert(extract<int>(annotations_of(^^fn)[1]) == 2);
static_assert(extract<int>(annotations_of(^^fn)[2]) == 3);
static_assert(extract<int>(annotations_of(^^fn)[3]) == 4);
static_assert(extract<int>(annotations_of(^^fn)[4]) == 5);
static_assert(extract<int>(annotations_of(^^fn)[5]) == 6);
