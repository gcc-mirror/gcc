/* { dg-do compile } */
/* { dg-require-effective-target naked_functions } */
/* { dg-options "-O0" } */

extern void g(int *x);

void __attribute__((naked)) f(void)
{
    int x = 0; /* { dg-error "cannot allocate stack for variable" } */
    g(&x);
}
