/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

/* Testing non-case labels.  */

int foo (int);

void
f1 (int i)
{
  switch (i)
    {
    case 0:
      foo (1);
    L1:
      foo (2);
    }

  switch (i)
    {
    case 0:
      foo (1); /* { dg-warning "statement may fall through" } */
    L2:
    case 2:
      foo (2);
    }

  switch (i)
    {
    case 0:
      foo (1); /* { dg-warning "statement may fall through" } */
    case 2:
    L3:
      foo (2);
    }

  switch (i)
    {
    case 0:
      foo (1); /* { dg-warning "statement may fall through" } */
    L4:
    case 2:
    L5:
      foo (2);
    }

  switch (i)
    {
    case 0:
      switch (i)
	{
	case 1:
	  foo (2);
	L6:
	  foo (3);
	}
    }

  switch (i)
    {
    case 0:
      switch (i)
	{
	case 1:
	  foo (2); /* { dg-warning "statement may fall through" } */
	L7:
	case 2:
	  foo (3);
	}
    }

  switch (i)
    {
    case 0:
      switch (i)
	{
	case 1:
	  foo (2); /* { dg-warning "statement may fall through" } */
	case 2:
	L8:
	  foo (3);
	}
    }
}
