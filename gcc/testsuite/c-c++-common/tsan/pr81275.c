/* PR sanitizer/81275 */
/* { dg-do compile } */
/* { dg-options "-Wreturn-type -fsanitize=thread" } */

int
f1 (int a, int b)
{
  switch (a)
    {
    case 0:
      switch (b)
        {
        case 5:
	  return 6;
	case 7:
	  return 8;
	default:
	  return 0;
	}
      break;
    default:
      return 0;
    }
}	/* { dg-bogus "control reaches end of non-void function" } */

int
f2 (int a, int b)
{
  switch (a)
    {
    case 0:
      switch (b)
        {
        case 5:
	  return 6;
	case 7:
	  return 8;
	default:
	  return 0;
	}
    default:
      return 0;
    }
}	/* { dg-bogus "control reaches end of non-void function" } */

int
f3 (int a, int b)
{
  switch (a)
    {
    case 0:
      switch (b)
        {
        case 5:
	  return 6;
	case 7:
	  return 8;
	case 8:
	  break;
	default:
	  return 0;
	}
      break;
    default:
      return 0;
    }
}	/* { dg-warning "control reaches end of non-void function" } */

int
f4 (int a, int b)
{
  switch (a)
    {
    case 0:
      switch (b)
        {
        case 5:
	  return 6;
	case 7:
	  return 8;
	}
      break;
    default:
      return 0;
    }
}	/* { dg-warning "control reaches end of non-void function" } */

int
f5 (int a, unsigned char b)
{
  switch (a)
    {
    case 0:
      switch (b)
        {
	case 0:
	  return 1;
	case 3 ... 10:
	  return 2;
	case 1 ... 2:
	  return 3;
	case 126 ... (unsigned char) ~0:
	  return 4;
	case 11 ... 125:
	  return 5;
	}
      break;
    default:
      return 0;
    }
}	/* { dg-bogus "control reaches end of non-void function" } */
