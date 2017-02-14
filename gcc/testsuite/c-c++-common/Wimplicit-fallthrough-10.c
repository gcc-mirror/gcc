/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void bar (int);

void
f (int i)
{
  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  break;
	}
      else if (i > 10)
	{
	  bar (1);
	  __attribute__((fallthrough));
	}
      else
	break;
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	bar (2);
      else if (i > 10)
	{
	  bar (3);
	  __attribute__((fallthrough));
	}
      else
	break;
      case 2:
	bar (4);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  break;
	}
      else if (i > 10) /* { dg-warning "statement may fall through" } */
	{
	  bar (1);
	}
      else
	break;
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  break;
	}
      else if (i > 10)
	{
	  bar (1);
	  break;
	}
      else
	break;
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  break;
	}
      else if (i > 10)
	{
	  bar (1);
	  break;
	}
      else
	bar (2); /* { dg-warning "statement may fall through" } */
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  __attribute__((fallthrough));
	}
      else if (i > 10)
	{
	  bar (1);
	  break;
	}
      else
	bar (2); /* { dg-warning "statement may fall through" } */
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  __attribute__((fallthrough));
	}
      else if (i > 10)
	{
	  bar (1);
	  __attribute__((fallthrough));
	}
      else
	break;
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  __attribute__((fallthrough));
	}
      else if (i > 10)
	{
	  bar (1);
	  __attribute__((fallthrough));
	}
      else
	bar (2); /* { dg-warning "statement may fall through" } */
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  __attribute__((fallthrough));
	}
      else if (i > 10) /* { dg-warning "statement may fall through" } */
	{
	  bar (1);
	  bar (2);
	}
      else
	__attribute__((fallthrough));
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)  /* { dg-warning "statement may fall through" } */
	{
	  bar (0);
	}
      else if (i > 10)
	{
	  bar (1);
	}
      else
	{
	  bar (1);
	  __attribute__((fallthrough));
	}
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  __attribute__((fallthrough));
	}
      else if (i > 10)
	{
	  bar (1);
	  break;
	}
      else
	{
	  bar (1);
	  __attribute__((fallthrough));
	}
      case 2:
	bar (99);
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	  bar (0);
	  break;
	}
      else if (i > 10) /* { dg-warning "statement may fall through" } */
	{
	  bar (1);
	}
      else
	{
	  bar (1);
	  __attribute__((fallthrough));
	}
      case 2:
	bar (99);
    }
}
