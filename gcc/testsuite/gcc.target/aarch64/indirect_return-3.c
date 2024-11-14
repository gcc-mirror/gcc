/* Check that mismatching attribute on function pointers is an error.  */
/* { dg-do compile } */

void f(void);
void (*f_ptr)(void) __attribute__((indirect_return)) = f; /* { dg-error {incompatible pointer type} } */

void g(void) __attribute__((indirect_return));
void (*g_ptr1)(void) = g; /* { dg-error {incompatible pointer type} } */
void (*g_ptr2)(void) __attribute__((indirect_return)) = g;
