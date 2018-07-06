// PR c++/83490
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-fipa-icf-functions -Og -maccumulate-outgoing-args" }

struct A {};
A operator < (A, A) { return A (); }
A operator > (A, A) { return A (); }
