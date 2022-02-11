/* PR middle-end/103365 */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-Wno-attributes=foo::_bar" } */
/* { dg-additional-options "-Wno-attributes=_foo::bar" } */

[[foo::_bar]] void foo (void);
[[_foo::bar]] void bar (void);
