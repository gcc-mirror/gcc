/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches -O2" } */

void
f (int *p)
{
  if (*p > 0)
    {
      if (x == 0) /* { dg-error "undeclared|not declared" } */
	*p = 5;
      else
	*p = 6;
    }
}

void
f2 (int *p)
{
  if (*p > 0)
    {
      if (*p > 2)
	*p = x; /* { dg-error "undeclared|not declared" } */
      else
	*p = 6;
    }
}

void
f3 (int *p)
{
  if (*p > 0)
    {
      if (*p > 2)
	*p = 8;
      else
	*p = x; /* { dg-error "undeclared|not declared" } */
    }
}

void
f4 (int *p)
{
  if (*p > 0)
    {
      if (x == 0) /* { dg-error "undeclared|not declared" } */
	*p = 5;
      else
	*p = 6;
    }
  else
    {
      if (x == 0) /* { dg-error "not declared" "" { target c++ } } */
	*p = 7;
      else
	*p = 6;
    }
}

void
f5 (int *p)
{
  if (*p > 0)
    {
      if (*p > 2)
	*p = x; /* { dg-error "undeclared|not declared" } */
      else
	*p = 6;
    }
  else
    {
      if (x == 0) /* { dg-error "not declared" "" { target c++ } } */
	*p = 5;
      else
	*p = 6;
    }
}

void
f6 (int *p)
{
  if (*p > 0)
    {
      if (*p > 2)
	*p = 8;
      else
	*p = x; /* { dg-error "undeclared|not declared" } */
    }
  else
    {
      if (x == 0) /* { dg-error "not declared" "" { target c++ } } */
	*p = 5;
      else
	*p = 6;
    }
}

void
f7 (int i)
{
  if (i > 5)
    ({ x++; }); /* { dg-error "undeclared|not declared" } */
  else
    ({ i++; });
}

void
f8 (int i)
{
  if (i > 5)
    ({ i++; });
  else
    ({ x++; }); /* { dg-error "undeclared|not declared" } */
}
