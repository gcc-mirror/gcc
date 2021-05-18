// PR c++/100580
// { dg-do compile }
// { dg-require-weak "" }
// { dg-options "-fdump-passes" }
// { dg-prune-output ".*" }

int foo;
static __typeof(foo) bar __attribute__((__weakref__("foo")));
