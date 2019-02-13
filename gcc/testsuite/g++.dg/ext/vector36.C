// PR target/89186
// { dg-do compile }
// { dg-options "-fnon-call-exceptions" }
// { dg-additional-options "-mno-sse" { target i?86-*-* x86_64-*-* } }

#include "vector27.C"
