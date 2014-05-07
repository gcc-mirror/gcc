/* PR middle-end/59011 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
foo (int m)
{
  int a[m];
  void
  bar (void)
  {
    {
      int
      baz (void)
      {
	return a[0];
      }
    }
    a[0] = 42;
  }
  bar ();
}
