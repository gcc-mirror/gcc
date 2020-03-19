/* PR c/94172 */
/* { dg-do compile } */
/* { dg-options "-Os -g -fshort-enums" } */

extern enum E e;
extern void bar (int a);
enum E { F };

void
foo (int a)
{
  int l = e;
  if (a)
    {
      __asm volatile ("nop");
      l = 0;
    }
  bar (l);
}
