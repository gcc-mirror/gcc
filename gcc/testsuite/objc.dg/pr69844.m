/* PR objc/69844 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

@class D;

void
foo (void)
{
  for (;;)
    ;
  D *d = (id) 0;
  (void) d;
}

void
bar (void)
{
  for (int D = 0; D < 30; D++)
    if (1)
      ;
  D *d = (id) 0;
  (void) d;
}
