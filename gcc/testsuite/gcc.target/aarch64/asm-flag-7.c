/* Test that "=@cc*" works with MEM_P RTX  */
/* PR target/102941 */
/* { dg-do compile } */
/* { dg-options "-O" } */

#ifndef __GCC_ASM_FLAG_OUTPUTS__
#error "missing preprocessor define"
#endif
int test_cmpu_x;

void f(long *);
long
test_cmpu_y() {
  long le;
  f(&le);
  __asm__("cmp %"
          "[x], %"
          "[y]"
          : "=@ccls"(le)
          : [x] ""(test_cmpu_x), [y] ""(test_cmpu_y));
    return le;
}
