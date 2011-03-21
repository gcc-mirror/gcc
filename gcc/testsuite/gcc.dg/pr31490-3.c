/* PR middle-end/31490 */
/* { dg-do compile } */
/* { dg-options "-fpic" { target fpic } } */
/* { dg-require-named-sections "" } */

const char *const x __attribute__((section("foo"))) = (const char *) 0;
const char *const g __attribute__((section("foo"))) = "bar";
