// { dg-additional-options -fmodules-ts }

import foo;
struct pthread_attr_t obj2;
pthread_attr_t obj1; // OK -- we see the implicit typedef

bob obj4;
// the structure tag is not exported.  We find the typedef-name, which
// is ill-formed
struct bob obj5;  // { dg-error "using typedef-name" }
