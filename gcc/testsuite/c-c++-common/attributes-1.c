/* { dg-do compile } */
/* { dg-prune-output "undeclared here \\(not in a function\\); did you mean .char..|\[^\n\r\]* was not declared in this scope" } */

void* my_calloc(unsigned, unsigned) __attribute__((alloc_size(1,bar))); /* { dg-warning "outside range" } */
void* my_realloc(void*, unsigned) __attribute__((alloc_size(bar))); /* { dg-warning "outside range" } */

typedef char vec __attribute__((vector_size(bar))); /* { dg-warning "ignored" } */

void f1(char*) __attribute__((nonnull(bar))); /* { dg-error "invalid operand" } */
void f2(char*) __attribute__((nonnull(1,bar))); /* { dg-error "invalid operand" } */

void foo(void);
void* my_calloc(unsigned, unsigned) __attribute__((alloc_size(1,foo))); /* { dg-warning "outside range" } */
void* my_realloc(void*, unsigned) __attribute__((alloc_size(foo))); /* { dg-warning "outside range" } */

typedef char vec __attribute__((vector_size(foo))); /* { dg-warning "ignored" } */

void f1(char*) __attribute__((nonnull(foo))); /* { dg-error "invalid operand" } */
void f2(char*) __attribute__((nonnull(1,foo))); /* { dg-error "invalid operand" } */

void g() __attribute__((aligned(foo))); /* { dg-error "invalid value|not an integer" } */
