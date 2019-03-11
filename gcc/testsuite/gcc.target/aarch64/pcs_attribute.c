/* { dg-do compile } */
 
/* Test that the assignment of f (with the attribute) to function pointer g
   (with no attribute) results in an error.  */

__attribute__((aarch64_vector_pcs)) void f(void);
void (*g)(void) = f; /* { dg-error "incompatible pointer type" } */
