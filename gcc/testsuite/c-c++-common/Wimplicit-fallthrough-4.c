/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void bar (int);

/* Test if with more elses.  */

void
f (int i)
{
  switch (i)
    {
    case 1:
      if (i > 5) /* { dg-warning "statement may fall through" } */
        bar (1);
      else if (i > 10)
	bar (2);
      else if (i > 15)
	bar (3);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5) /* { dg-warning "statement may fall through" } */
	bar (1);
      else if (i > 10)
	bar (2);
      else if (i > 15)
	bar (3);
      else
	bar (4);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
        return;
      else if (i > 10) /* { dg-warning "statement may fall through" } */
	bar (2);
      else if (i > 15)
	bar (3);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
        return;
      else if (i > 10) /* { dg-warning "statement may fall through" } */
	bar (2);
      else if (i > 15)
	bar (3);
      else
	bar (4);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5) /* { dg-warning "statement may fall through" } */
        bar (1);
      else if (i > 10)
	return;
      else if (i > 15)
	bar (3);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5) /* { dg-warning "statement may fall through" } */
        bar (1);
      else if (i > 10)
	return;
      else if (i > 15)
	bar (3);
      else
	bar (4);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5) /* { dg-warning "statement may fall through" } */
        bar (1);
      else if (i > 10)
	bar (4);
      else if (i > 15)
	return;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5) /* { dg-warning "statement may fall through" } */
        bar (1);
      else if (i > 10)
	bar (4);
      else if (i > 15)
	return;
      else
	bar (4);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
        return;
      else if (i > 10)
	return;
      else if (i > 15) /* { dg-warning "statement may fall through" } */
	bar (3);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
        return;
      else if (i > 10)
	return;
      else if (i > 15) /* { dg-warning "statement may fall through" } */
	bar (3);
      else
	bar (4);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
        return;
      else if (i > 10) /* { dg-warning "statement may fall through" } */
	bar (2);
      else if (i > 15)
	return;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
        return;
      else if (i > 10) /* { dg-warning "statement may fall through" } */
	bar (2);
      else if (i > 15)
	return;
      else
	bar (4);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5) /* { dg-warning "statement may fall through" } */
        bar (1);
      else if (i > 10)
	return;
      else if (i > 15)
	return;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5) /* { dg-warning "statement may fall through" } */
        bar (1);
      else if (i > 10)
	return;
      else if (i > 15)
	return;
      else
	bar (4);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
       return;
      else if (i > 10)
	return;
      else if (i > 15) /* { dg-warning "statement may fall through" } */
	return;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
       return;
      else if (i > 10)
	return;
      else if (i > 15)
	return;
      else
	bar (4); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i > 5)
       return;
      else if (i > 10)
	return;
      else if (i > 15)
	return;
      else
	return;
    case 2:
      __builtin_abort ();
    }
}
