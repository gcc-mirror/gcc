/* PR c/79152 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void foo (int);

void
f (int i)
{
  switch (i)
    {
    case 0:
      foo (0);
l1:
      foo (1);
    }

  switch (i)
    {
    case 0:
      foo (0);
l2:;
    }

  switch (i)
    {
    case 0:
      foo (0);
l3:
l4:
      foo (1);
    }

  switch (i)
    {
    case 0:
      foo (0);
l5:
l6:;
    }

  switch (i)
    {
    case 0:
      foo (0); /* { dg-warning "statement may fall through" } */
l7:
l8:
    case 1:
      foo (1);
    }

  switch (i)
    {
    case 0:
      foo (0); /* { dg-warning "statement may fall through" } */
l9:
    case 1:
l10:
      foo (1);
    }
}
