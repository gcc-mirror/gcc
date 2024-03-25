// DR 2406 - [[fallthrough]] attribute and iteration statements
// PR c++/107571
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic-errors -Wimplicit-fallthrough" }

void bar ();
void baz ();
void qux ();

void
foo (int n)
{
  switch (n)
    {
    case 1:
    case 2:
      bar ();
      [[fallthrough]];
    case 3:
      do
	{
	  [[fallthrough]];	// { dg-error "attribute 'fallthrough' not preceding a case label or default label" }
	}
      while (false);
    case 6:
      do
	{
	  [[fallthrough]];	// { dg-error "attribute 'fallthrough' not preceding a case label or default label" }
	}
      while (n--);
    case 7:
      while (false)
	{
	  [[fallthrough]];	// { dg-error "attribute 'fallthrough' not preceding a case label or default label" }
	}
    case 5:
      baz ();			// { dg-warning "this statement may fall through" }
    case 4:			// { dg-message "here" }
      qux ();
      [[fallthrough]];		// { dg-error "attribute 'fallthrough' not preceding a case label or default label" }
    }
}

void
corge (int n)
{
  switch (n)
    {
    case 1:
      {
	int i = 0;
	do
	  {
	    [[fallthrough]];	// { dg-error "attribute 'fallthrough' not preceding a case label or default label" }
	  }
	while (false);
      }
    case 2:
      bar ();
      break;
    default:
      break;
    }
}

void
fred (int n)
{
  switch (n)
    {
    case 1:
      {
	int i = 0;
	[[fallthrough]];
      }
    case 2:
      bar ();
      break;
    default:
      break;
    }
}
