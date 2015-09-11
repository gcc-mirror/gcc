/* { dg-do compile } */
/* { dg-require-effective-target naked_functions } */
/* { dg-options "-O0" } */

__attribute__ ((naked))
void __data_abort(void)
{
  long foo; /* { dg-error "cannot allocate stack for variable" } */
  long* bar = &foo;
}
