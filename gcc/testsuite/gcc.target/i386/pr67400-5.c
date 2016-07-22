/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fno-plt" } */

extern void foo (void);
extern void bar (int, int, int, int, int, int, void *);

void
x (void)
{
  bar (1, 2, 3, 4, 5, 6, foo);
}
