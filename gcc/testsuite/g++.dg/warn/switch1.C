// { dg-do compile { target { int32plus } } }

signed char sc;

void
foo (void)
{
  switch (sc)
    {
    case 1:
    case 2 * __SCHAR_MAX__ + 3:		// { dg-warning "case label value exceeds maximum" }
    case - 2 * __SCHAR_MAX__ - 1:	// { dg-warning "case label value is less than minimum" }
      break;
    }
}
