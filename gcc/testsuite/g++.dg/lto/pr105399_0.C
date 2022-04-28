// PR lto/105399
// { dg-lto-do link }
// { dg-lto-options { { -fPIC -flto -Ofast } } }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-extra-ld-options "-shared -O2" }

struct S { S (); };
S::S () {}
