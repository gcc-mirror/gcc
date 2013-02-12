// Header file used by mv12.C and mv12-aux.C.
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "" }

int foo () __attribute__ ((target ("default")));
int foo () __attribute__ ((target ("sse4.2")));
