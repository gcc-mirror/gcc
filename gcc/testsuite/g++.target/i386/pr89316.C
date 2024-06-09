// PR target/89316
// { dg-do compile }
// { dg-require-effective-target split_stack }
// { dg-options "-fsplit-stack -mforce-indirect-call" }

struct foo { foo (); } foobar;
