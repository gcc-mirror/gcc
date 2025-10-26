/* PR c/122427 */
/* { dg-do compile { target { "i?86-*-* x86_64-*-*" } } } */
/* { dg-options "-O2 -fcf-protection" } */

extern void foo (void);

__attribute__((nocf_check))
void
foo (void) /* { dg-error "conflicting types" } */
{
}
