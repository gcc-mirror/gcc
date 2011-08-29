/* PR middle-end/48722 */
/* { dg-do compile } */
/* { dg-options "-Os -mno-push-args" } */

extern long long a;
extern int b;
void bar (int, long long);

void
foo (void)
{
  bar (a > 0x85, b);
}
