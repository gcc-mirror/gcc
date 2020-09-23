/* PR c/97125 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

void foo (void);

void
fn1 (void)
{
  if (0)
    foo ();
  else
    switch (0);
}

void
fn2 (void)
{
  if (0)
    foo ();
  else
    while (0);
}

void
fn3 (void)
{
  if (0)
    foo ();
  else
    for (;;);
}
