/* PR c/70436 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus -Wparentheses" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

int a, b, c;
void bar (void);
void baz (void);

void
f1 (void)
{
  if (a) /* { dg-warning "ambiguous" } */
    _Cilk_for (int i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a)
    _Cilk_for (int i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    _Cilk_for (int i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
      }
  else
    baz ();
}

void
f2 (void)
{
  if (a) /* { dg-warning "ambiguous" } */
    #pragma cilk grainsize = 2
    _Cilk_for (int i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a)
    #pragma cilk grainsize = 2
    _Cilk_for (int i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    #pragma cilk grainsize = 2
    _Cilk_for (int i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
      }
  else
    baz ();
}
