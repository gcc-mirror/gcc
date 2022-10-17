// { dg-additional-options "-fmodules-ts" }
module foo;

struct C;
struct B { using type = C; };
struct C { static_assert(A<B>::value == 42); };
