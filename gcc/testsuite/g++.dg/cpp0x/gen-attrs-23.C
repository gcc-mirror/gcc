// PR c++/28112
// { dg-do compile { target c++11 } }

int i       [[gnu::init_priority(;)]];  // { dg-error "before" }
int j       [[gnu::vector_size(;)]];    // { dg-error "before" }
int k       [[gnu::visibility(;)]];     // { dg-error "before" }
struct A {} [[gnu::aligned(;)]];        // { dg-error "before" }
struct B {} [[gnu::mode(;)]];           // { dg-error "before" }
void foo()  [[gnu::alias(;)]];          // { dg-error "before" }
void bar()  [[gnu::nonnull(;)]];        // { dg-error "before" }
void baz()  [[gnu::section(;)]];        // { dg-error "before" }
