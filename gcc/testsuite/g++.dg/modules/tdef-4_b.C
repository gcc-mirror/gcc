// { dg-additional-options -fmodules-ts }
module foo;

pthread_attr_t obj1;
struct pthread_attr_t obj2;

bob obj3;
struct bob obj4; // OK -- we see the implicit typedef
