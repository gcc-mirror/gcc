// PR c++/28112
// { dg-do compile }

int i       __attribute__((init_priority(;)));  // { dg-error "before" }
int j       __attribute__((vector_size(;)));    // { dg-error "before" }
int k       __attribute__((visibility(;)));     // { dg-error "before" }
struct A {} __attribute__((aligned(;)));        // { dg-error "before" }
struct B {} __attribute__((mode(;)));           // { dg-error "before" }
void foo()  __attribute__((alias(;)));          // { dg-error "before" }
void bar()  __attribute__((nonnull(;)));        // { dg-error "before" }
void baz()  __attribute__((section(;)));        // { dg-error "before" }
