/* PR target/89474 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

int a;
void foo (double);
int baz (void);

void
bar (void)
{
  while (baz ())
    foo (a);
}
