/* { dg-do compile } */
/* { dg-options "-O2 -Wunreachable-code" } */

extern void foo (void);
extern void baz (void);

void bar (int i)
{
  if (i < 2)
    {
      baz ();
      return;
    }
  else
    {
      if (i >= 4 && i <= 5)
        foo ();
      return;
    }

  baz ();	/* { dg-warning "will never be executed" "" } */
  baz ();
  baz ();
}
