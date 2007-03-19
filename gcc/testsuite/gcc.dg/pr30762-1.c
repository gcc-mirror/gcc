/* PR c/30762 */
/* { dg-do compile } */
/* { dg-options "--combine -O3" } */
/* { dg-additional-sources pr30762-2.c } */

typedef struct { int i; } D;
extern void foo (D);

void
bar (void)
{
  D d;
  d.i = 1;
  foo (d);
}
