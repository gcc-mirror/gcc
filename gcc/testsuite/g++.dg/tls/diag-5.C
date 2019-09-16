// PR c++/30536
// Invalid __thread specifiers.
// { dg-require-effective-target tls }

struct A { __thread register int i; }; // { dg-error "12:multiple storage classes|storage class specified" }
