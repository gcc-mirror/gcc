/* Test that we do not ice in thumb1 mode */
/* { dg-do compile } */
/* { dg-options "-march=armv4t" } */

void __attribute__((target("arm"))) f(char *out)
{
  asm("" : "=@ccne"(out[0]));
}

void __attribute__((target("thumb"))) g(char *out)
{
  asm("" : "=@ccne"(out[0]));  /* { dg-message "asm flags not supported" } */
}
