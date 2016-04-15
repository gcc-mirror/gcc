/* PR c/70436 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus -Wparentheses" } */

int a, b, c;
void bar (void);
void baz (void);

void
f1 (void)
{
  int i;

  if (a) /* { dg-warning "ambiguous" } */
    #pragma simd
    for (i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a)
    #pragma simd
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    #pragma simd
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
      }
  else
    baz ();
}
