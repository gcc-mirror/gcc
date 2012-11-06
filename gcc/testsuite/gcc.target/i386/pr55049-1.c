/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC -mx32" } */

extern void __morestack_fail (const char *msg);
void
foo (void)
{
  static const char msg[] = "munmap of stack space failed: errno ";
  __morestack_fail (msg);
}
