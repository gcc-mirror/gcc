/* PR middle-end/94189 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

const char a[] = { 'a', 'b', 'c', 'd' };/* { dg-message "declared here" } */

int
foo (void)
{
  return __builtin_strnlen (a, 5);	/* { dg-warning "specified bound 5 exceeds the size 4 of unterminated array" } */
}
