/* PR sanitizer/85213 */
/* { dg-do compile } */
/* Pass -gno-statement-frontiers to work around
   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100733 :
   without it the IR coming from the front end may be different with and without
   debug information turned on. That may cause e.g., different discriminator values
   and -fcompare-debug failures. */
/* { dg-options "-O1 -fsanitize=undefined -fcompare-debug -gno-statement-frontiers" } */

int
foo (int x)
{
  return (__builtin_expect (({ x != 0; }) ? 0 : 1, 3) == 0) * -1 << 0;
}
