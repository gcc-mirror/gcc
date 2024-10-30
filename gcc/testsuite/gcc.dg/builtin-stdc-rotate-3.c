/* { dg-do compile } */
/* { dg-options "-funsigned-char" } */

void
foo (void)
{
  __builtin_stdc_rotate_left ((char) 0, 0);			/* { dg-error "argument 1 in call to function '__builtin_stdc_rotate_left' has 'char' type" } */
  __builtin_stdc_rotate_right ((char) 0, 0);			/* { dg-error "argument 1 in call to function '__builtin_stdc_rotate_right' has 'char' type" } */
}
