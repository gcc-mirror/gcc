/* PR c/66322 */
/* { dg-do compile } */

#ifndef __cplusplus
# define bool _Bool
# define true 1
# define false 0
#endif

void
nowarn (bool b)
{
  switch (b)
    ;

  switch (b)
    {
    case true:
    case false:
      break;
    }

  switch (b)
    {
    case true:
      break;
    }

  switch (b)
    {
    case true:
    default:
      break;
    }

  switch (b)
    {
    case false:
      break;
    }

  switch (b)
    {
    case false:
    default:
      break;
    }

  switch (b)
    {
    default:
      break;
    }

  switch (b)
    {
    case false ... true:
      break;
    }

  switch (b)
    {
    case 1:
      switch (b)
	{
	case true:
	default:
	  break;
	}
      default:
	break;
    }
}

void
warn (bool b)
{
  switch (b)  /* { dg-warning "switch condition has" } */
    {
    case true:
    case false:
    default:
      break;
    }

  switch (b)  /* { dg-warning "switch condition has" } */
    {
    case false ... true:
    default:
      break;
    }
}

void
warn2 (int n)
{
  switch (n == 2)  /* { dg-warning "switch condition has" } */
    {
    case 0 ... 2: /* { dg-warning "upper value" "" { target c++ } } */
    default:
      break;
    }

  switch (n == 2)  /* { dg-warning "switch condition has" } */
    {
    case 1 ... 10: /* { dg-warning "upper value" "" { target c++ } } */
    default:
      break;
    }

  switch (n == 2) /* { dg-warning "switch condition has" } */
    {
      case 2: /* { dg-warning "case label" "" { target c++ } } */
	break;
    }

  switch (n == 2) /* { dg-warning "switch condition has" } */
    {
      case 0:
      case 1:
      case -1: /* { dg-warning "case label" "" { target c++ } } */
	break;
    }

  switch (n == 2) /* { dg-warning "switch condition has" } */
    {
      case -1 ... 1: /* { dg-warning "lower value" "" { target c++ } } */
	break;
    }

  switch (n == 2) /* { dg-warning "switch condition has" } */
    {
      case -1 ... 0: /* { dg-warning "lower value" "" { target c++ } } */
      default:
	break;
    }

  switch (n == 2) /* { dg-warning "switch condition has" } */
    {
      case -10 ... -1: /* { dg-warning "case label" "" { target c++ } } */
      default:
	break;
    }
}
