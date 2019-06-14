// { dg-additional-options -fmodules-ts }

import foo;
struct pthread_attr_t obj2;
pthread_attr_t obj1; // OK -- we see the implicit typedef

bob obj4;
struct bob obj5;  // { dg-error "using typedef-name" }
