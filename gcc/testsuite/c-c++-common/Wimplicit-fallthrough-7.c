/* PR c/7652 */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void bar (int);
extern int bar2 (void);
extern int *map;
void
f (int i)
{
  switch (i)
    {
    case 1:
      bar (0); /* { dg-warning "statement may fall through" } */
      static int i = 10;
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      { /* { dg-warning "statement may fall through" "" { target c } } */
	int a[i]; /* { dg-warning "statement may fall through" "" { target c++ } } */
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      for (int j = 0; j < 10; j++) /* { dg-warning "statement may fall through" "" { target c } } */
	map[j] = j; /* { dg-warning "statement may fall through" "" { target c++ } } */
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      do
	bar (2);
      while (--i); /* { dg-warning "statement may fall through" } */
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      {
	switch (i + 2) /* { dg-warning "statement may fall through" } */
	  case 4:
	    bar (1);
	  case 5:
	    bar (5);
	    return;
      }
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:;
    case 2:;
    }

  switch (i)
    {
    }

  switch (i)
    {
    case 1:
      if (i & 1) /* { dg-warning "statement may fall through" } */
	{
	  bar (23);
	  break;
	}
    case 2:
      bar (99);
    }

  switch (i)
    {
    case 1:
      if (i > 9) /* { dg-warning "statement may fall through" } */
	{
	  bar (9);
	  if (i == 10)
	    {
	      bar (10);
	      break;
	    }
	}
    case 2:
      bar (99);
    }

  int r;
  switch (i)
    {
    case 1:
      r = bar2 ();
      if (r) /* { dg-warning "statement may fall through" } */
	break;
      case 2:
	bar (99);
    }

  switch (i)
    {
      case 1:
	r = bar2 ();
	if (r)
	  return;
	if (!i) /* { dg-warning "statement may fall through" } */
	  return;
      case 2:
	bar (99);
    }
}
