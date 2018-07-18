// { dg-do link { target c++11 } }
// { dg-require-effective-target tls }
// { dg-add-options tls }
// { dg-additional-sources pr77285-2.C }

struct __attribute__((abi_tag("tag"))) X { ~X () {} int i = 0; };
thread_local X var1;
X var2;
