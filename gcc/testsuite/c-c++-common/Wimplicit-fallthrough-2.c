/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void bar (int);

/* Test if without else.  */

void
f (int i)
{
  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	bar (1);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i) /* { dg-warning "statement may fall through" } */
	return;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	return;
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)  /* { dg-warning "statement may fall through" } */
	goto L1;
    case 2:
L1:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)  /* { dg-warning "statement may fall through" } */
	goto L2;
L2:
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	goto L3;
      break;
    case 2:
L3:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	goto L4;
      break;
L4:
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)  /* { dg-warning "statement may fall through" } */
	if (i > 9)
	  bar (1);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	if (i > 9)
	  bar (1);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      { int a; }
      {
      if (i)  /* { dg-warning "statement may fall through" } */
	if (i > 9)
	  bar (1);
      }
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      bar (2); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
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
      bar (2); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	return;
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
      if (i)
	bar (2);
      if (i)
	bar (3);
      bar (4); /* { dg-warning "statement may fall through" } */
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      if (i)
	bar (2);
      if (i) /* { dg-warning "statement may fall through" } */
	bar (3);
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      if (i)
	bar (2);
      if (i)
	bar (3);
      bar (4);
      break;
    case 2:
      __builtin_abort ();
    }

  switch (i)
    {
    case 1:
      if (i)
	bar (1);
      if (i)
	bar (2);
      if (i)
	bar (3);
      break;
    case 2:
      __builtin_abort ();
    }
}
