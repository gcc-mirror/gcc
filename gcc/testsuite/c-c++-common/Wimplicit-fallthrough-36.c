/* PR c/79153 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

int
test (int v1, int v2)
{
  switch (v1)
    {
    case 3:
      switch (v2)	/* { dg-warning "this statement may fall through" } */
	{
	case 1:
	  return 28;
	case 2:
	  return 38;
	case 3:
	  return 88;
	default:
	  break;
	}
    case 4:		/* { dg-message "here" } */
      return 168;
    case 5:
      switch (v2)	/* { dg-warning "this statement may fall through" } */
	{
	case 4:
	  break;
	case 5:
	  return 38;
	case 6:
	  return 88;
	}
    case 6:		/* { dg-message "here" } */
      return 169;
    case 7:
      switch (v2)	/* { dg-warning "this statement may fall through" } */
	{
	case 7:
	  return 38;
	case 8:
	  return 88;
	}
    case 8:		/* { dg-message "here" } */
      return 170;
    case 9:
      switch (v2)	/* { dg-bogus "this statement may fall through" } */
	{
	case 9:
	  return 38;
	case 10:
	  return 88;
	default:
	  return 89;
	}
    case 10:
      return 171;
    case 11:
      switch (v2)	/* { dg-bogus "this statement may fall through" } */
	{
	case -__INT_MAX__ - 1 ... 31:
	  return 38;
	case 32:
	  return 88;
	case 33 ... __INT_MAX__:
	  return 89;
	}
    case 12:
      return 172;
    }
  return -1;
}
