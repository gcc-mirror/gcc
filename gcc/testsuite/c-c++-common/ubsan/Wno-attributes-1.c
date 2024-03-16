/* PR c/112339 */
/* { dg-do compile { target { c++11 || c } } } */
/* { dg-options "-Wno-attributes=foo::no_sanitize -fsanitize=undefined" } */
/* { dg-additional-options "-std=c2x" { target c } } */

[[foo::no_sanitize("bounds")]] void
foo (void)
{
}
