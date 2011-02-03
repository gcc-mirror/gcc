/* PR middle-end/31490 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -fpic" { target fpic } } */
/* { dg-require-named-sections "" } */

const char *const x __attribute__((section("foo"))) = "";
const char *const g __attribute__((section("foo"))) = (const char *) 0;
const char *const y __attribute__((section("bar"))) = (const char *) 0;
const char *const h __attribute__((section("bar"))) = "bar";
