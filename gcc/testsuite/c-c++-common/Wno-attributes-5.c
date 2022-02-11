/* PR middle-end/103365 */
/* { dg-do compile { target { c || c++11 } } } */

#pragma GCC diagnostic ignored_attributes "foo::_bar"
#pragma GCC diagnostic ignored_attributes "_foo::bar"

[[foo::_bar]] void foo (void);
[[_foo::bar]] void bar (void);
