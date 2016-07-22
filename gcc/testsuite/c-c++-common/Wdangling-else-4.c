/* { dg-do compile } */
/* { dg-options "-Wdangling-else" } */

void bar (int);

void
foo (int a, int b, int c)
{
  if (a)	/* { dg-warning "suggest explicit braces to avoid ambiguous .else." } */
    switch (b)
      case 0:
	if (c)
	  bar (1);
  else
    bar (2);
}

void
baz (int a, int b, int c)
{
  if (a)
    switch (b)
      {
      case 0:
	if (c)
	  bar (1);
      }
  else
    bar (2);
}

