/* PR c/103649 */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-Wno-attributes=foo::bar" } */
/* { dg-additional-options "-Wno-attributes=baz::" } */
/* { dg-additional-options "-Wno-attributes=womp::womp" } */
/* { dg-additional-options "-Wno-attributes=qux::foo" } */
/* { dg-additional-options "-Wno-attributes=vendor::assume" } */

[[vendor::assume(1 + 1 == 2)]];
[[foo::bar(1, 2)]];
[[baz::bar(1, 2)]];
[[foo::bar(1, 2)]] void f1();
[[baz::bar(1, 2)]] void f2();
[[qux::foo({t})]] void f3(); 
[[womp::womp (another::directive (threadprivate (t)))]] void f4();
[[womp::womp (another::directive (threadprivate (t)))]];
