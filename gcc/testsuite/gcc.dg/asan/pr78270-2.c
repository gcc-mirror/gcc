/* PR sanitizer/78270 */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-switch-unreachable" } */

int a;
void
fn1 ()
{
  switch (a)
    {
      char b;
    case 8:
      &b;
      switch (0)
	;
    }
}

