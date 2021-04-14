/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void bar (int);

/* Test nested scopes.  */

void
f (int i)
{
  switch (i)
    {
    case 1:
      {
	int j;
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 10; /* { dg-warning "statement may fall through" } */
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int k = 9;
	k++;
	{
	  int j = 10;
	  j++; /* { dg-warning "statement may fall through" } */
	}
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int k = 9;
	k++;
	{
	  int j = 10;
	  j++;
	  {
	    bar (1); /* { dg-warning "statement may fall through" } */
	  }
	}
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	__attribute__((fallthrough));
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	{
	  int k = j + 5;
	  bar (k);
	  __attribute__((fallthrough));
	}
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	return;
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	goto L1;
      }
L1:
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      { /* { dg-warning "statement may fall through" "" { target c } } */
	int j = 0;
	bar (j);
	if (j == 8)
	  return;
      } /* { dg-warning "statement may fall through" "" { target c++ } } */
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	if (j == 8)
	  return;
	else
	  return;
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      { /* { dg-warning "statement may fall through" "" { target c } } */
	int j = 0;
	bar (j);
	if (j == 8)
	  bar (1);
	else
	  return;
      } /* { dg-warning "statement may fall through" "" { target c++ } } */
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	if (j == 8)
	  return;
	else
	  bar (2); /* { dg-warning "statement may fall through" } */
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      { /* { dg-warning "statement may fall through" "" { target c } } */
	int j = 0;
	bar (j);
	if (j == 8)
	  bar (1);
	else
	  bar (2);
      } /* { dg-warning "statement may fall through" "" { target c++ } } */
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	if (j == 8)
	  return;
      }
      break;
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	if (j == 8)
	  return;
	else
	  return;
      }
      break;
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	if (j == 8)
	  bar (1);
	else
	  return;
      }
      break;
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	if (j == 8)
	  return;
	else
	  bar (2);
      }
      break;
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	if (j == 8)
	  bar (1);
	else
	  bar (2);
      }
      break;
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 9;
	while (1);
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      { /* { dg-warning "statement may fall through" "" { target c } } */
	int j = 9;
	switch (j);
      } /* { dg-warning "statement may fall through" "" { target c++ } } */
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	int j = 0;
	bar (j);
	if (j == 8)
	  bar (1);
	else
	  bar (2);
	__attribute__((fallthrough));
      }
    case 2:
      bar (99);
    }
}
