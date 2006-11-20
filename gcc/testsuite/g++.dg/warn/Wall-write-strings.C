// PR 8586
// { dg-do compile }
// { dg-options "-Wall" }

char* foo = "foo";              // { dg-warning "" }
const char* bar = "bar";

