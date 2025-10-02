/* { dg-do compile { target aarch64*-*-* arm*-*-* powerpc*-*-* s390*-*-* x86_64-*-* } } */

/* For the non existing variable we are faced with an error mark node during
   gimplify_asm_expr().  */

void test (void)
{
  __asm__ __volatile__ ("" : "={2}" (non_existing_var)); /* { dg-error {'non_existing_var' undeclared} } */
}
