/* { dg-do compile } */
/* { dg-prune-output "undeclared here \\(not in a function\\)|\[^\n\r\]* was not declared in this scope" } */

void* my_calloc(unsigned, unsigned) __attribute__((alloc_size(1,bar))); /* { dg-warning ".alloc_size. attribute argument 2 is invalid" } */
void* my_realloc(void*, unsigned) __attribute__((alloc_size(bar))); /* { dg-warning ".alloc_size. attribute argument is invalid" } */

typedef char vec __attribute__((vector_size(bar))); /* { dg-warning "ignored" } */

void f1(char*) __attribute__((nonnull(bar))); /* { dg-warning ".nonnull. attribute argument is invalid" } */

void f2(char*) __attribute__((nonnull(1,bar))); /* { dg-warning ".nonnull. attribute argument 2 is invalid" } */

void foo(int);
void* my_calloc(unsigned, unsigned) __attribute__((alloc_size(1,foo))); /* { dg-warning ".alloc_size. attribute argument 2 has type .void\\\(int\\\)." } */
void* my_realloc(void*, unsigned) __attribute__((alloc_size(foo))); /* { dg-warning ".alloc_size. attribute argument has type .void ?\\\(int\\\)" } */

typedef char vec __attribute__((vector_size(foo))); /* { dg-warning "ignored" } */

void f1(char*) __attribute__((nonnull(foo))); /* { dg-warning ".nonnull. attribute argument has type .void ?\\\(int\\\)." } */
void f2(char*) __attribute__((nonnull(1,foo))); /* { dg-warning ".nonnull. attribute argument 2 has type .void ?\\\(int\\\)." } */

void g() __attribute__((aligned(foo))); /* { dg-error "invalid value|not an integer" } */
