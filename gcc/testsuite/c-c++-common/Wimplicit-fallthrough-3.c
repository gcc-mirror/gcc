/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void bar (int);

/* Test if with else.  */

void
f (int i)
{
  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	bar (1);
      else
	bar (2);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      else
	bar (2);
      bar (3); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        return;
      else
	bar (2); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        return;
      else
	bar (2);
      bar (3); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
        bar (1);
      else
	return;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      else
	return;
      bar (3); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	return;
      else
	return;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	return;
      else
	return;
      bar (3); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	{
          bar (1);
          bar (2);
          bar (3);
          bar (4);
	}
      else
	{
          bar (5);
          bar (6);
          bar (7);
          bar (8);
	}
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	{
          bar (1);
          bar (2);
          bar (3);
          bar (4);
	}
      else
	{
          bar (5);
          bar (6);
          bar (7);
          bar (8);
	}
      bar (9); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
        {
	}
      else
	bar (2);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	bar (1);
      else
	{
	}
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	{
	}
      else
	{
	}
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	return;
      else
	{
	}
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	{
	}
      else
	return;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        goto L1;
      else
	bar (2); /* { dg-warning "statement may fall through" } */
    case 2:
L1:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        goto L2;
      else
	bar (2); /* { dg-warning "statement may fall through" } */
L2:
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	bar (1);
      else
        goto L3;
    case 2:
L3:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	bar (1);
      else
        goto L4;
L4:
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        goto L5;
      else
        goto L5;
L5:
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        bar (1);
      else
	bar (2);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        bar (1);
      else
	bar (2);
      bar (3);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        return;
      else
	bar (2);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        return;
      else
	bar (2);
      bar (3);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        bar (1);
      else
	return;
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      else
	return;
      bar (3);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	return;
      else
	return;
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	return;
      else
	return;
      bar (3);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	{
          bar (1);
          bar (2);
          bar (3);
          bar (4);
	}
      else
	{
          bar (5);
          bar (6);
          bar (7);
          bar (8);
	}
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	{
          bar (1);
          bar (2);
          bar (3);
          bar (4);
	}
      else
	{
          bar (5);
          bar (6);
          bar (7);
          bar (8);
	}
      bar (9);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        {
	}
      else
	bar (2);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      else
	{
	}
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	}
      else
	{
	}
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	return;
      else
	{
	}
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	{
	}
      else
	return;
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        goto L6;
      else
	bar (2);
      break;
    case 2:
L6:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        goto L7;
      else
	bar (2);
      break;
L7:
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      else
        goto L8;
      break;
    case 2:
L8:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      else
        goto L9;
      break;
L9:
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
        goto L10;
      else
        goto L10;
      break;
L10:
    case 2:
      __builtin_abort ();
    }
}
