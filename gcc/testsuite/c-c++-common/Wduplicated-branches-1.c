/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches -O2" } */

extern void foo (int);
extern int g;
extern int a[10];

int
f (int i, int *p)
{
  const int j = 0;
  if (j == 0)
    {
      if (i > 10) /* { dg-warning "this condition has identical branches" } */
	/* Optimizers can figure out that this is 1.  */
	*p = j * 2 + 1;
      else
	*p = 1;
    }

  if (i)
    ;
  else
    ;

  if (i == 0) /* { dg-warning "this condition has identical branches" } */
    return 0;
  else
    return 0;

  if (i == 1) /* { dg-warning "this condition has identical branches" } */
    {
      g = 10;
    }
  else
    {
      g = 10;
    }

  const char *s;
  if (i == 2) /* { dg-warning "this condition has identical branches" } */
    s = "foo";
  else
    s = "foo";

  if (i == 3) /* { dg-warning "this condition has identical branches" } */
    g = a[i];
  else
    g = a[i];

  if (i == 4) /* { dg-warning "this condition has identical branches" } */
    return i ? 1 : g;
  else
    return i ? 1 : g;

  if (i == 5) /* { dg-warning "this condition has identical branches" } */
    {
      {
	{
	  {
	    g++;
	  }
	}
      }
    }
  else
    {
      {
	{
	  {
	    g++;
	  }
	}
      }
    }

  if (i == 6) /* { dg-warning "this condition has identical branches" } */
    g = i * 6;
  else
    g = i * 6;

  /* Don't warn.  */
  if (i == 7)
    g = i / 6;
  else
    g = 6 / i;

  if (i == 8) /* { dg-warning "this condition has identical branches" } */
    return i * 8 * i * 8;
  else
    return 8 * i * 8 * i;


  if (i == 9) /* { dg-warning "this condition has identical branches" } */
    {
      p++;
      return *p;
    }
  else
    {
      p++;
      return *p;
    }

  /* Don't warn.  */
  if (i == 10)
    return *++p;
  else
    return ++*p;

  if (i == 11) /* { dg-warning "this condition has identical branches" } */
    {
      foo (6);
    }
  else
    {
      foo (6);
    }

  if (i == 12) /* { dg-warning "this condition has identical branches" } */
    {
      foo (6 + i), foo (2);
    }
  else
    {
      foo (6 + i), foo (2);
    }

  if (i == 13) /* { dg-warning "this condition has identical branches" } */
    p += (g + 1);
  else
    p += (g + 1);

  if (i == 14) /* { dg-warning "this condition has identical branches" } */
    {
      foo (7);
      *p = 0;
      foo (9);
    }
  else
    {
      foo (7);
      *p = 0;
      foo (9);
    }

  if (i == 15) /* { dg-warning "this condition has identical branches" } */
    p += (g + (1 + 2));
  else
    p += (g + (1 + 1 + 1));

  if (i == 16) /* { dg-warning "this condition has identical branches" } */
    foo (10 + g);
  else
    foo (g + 10);

  if (i == 17) /* { dg-warning "this condition has identical branches" } */
    ({ foo (i); });
  else
    ({ foo (i); });

  if (i == 18)
    {
      if (i == 19)
	{
	  if (i == 20) /* { dg-warning "this condition has identical branches" } */
	    foo (++i);
	  else
	    foo (++i);
	}
    }

  /* Don't warn.  */
  if (i == 21)
    {
      foo (1);
      foo (2);
    }
  else
    {
      foo (2);
      foo (1);
    }

  return 0;
}
