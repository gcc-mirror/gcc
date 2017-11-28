// PR sanitizer/81875
// { dg-do compile }
// { dg-options "-Wreturn-type" }

struct C { C (); ~C (); };

int
f1 (int a, int b, int c)
{
  C f;
  switch (a)
    {
    case 0:
      switch (b)
        {
	case 13:
	  return 7;
	case 24:
	  if (c == 5)
	    break;
	  return 19;
	default:
	  return 0;
	}
      break;
    default:
      return 0;
    case 9:
      return 17;
    }
}	// { dg-warning "control reaches end of non-void function" }

int
f2 (int a, int b, int c, int d)
{
  C f;
  switch (a)
    {
    case 0:
      switch (b)
        {
	case 13:
	  while (c >= 10)
	    {
	      if (c == d)
		break;
	      c--;
	    }
	  return 7;
	case 29:
	  switch (d)
	    {
	    case 35:
	      break;
	    default:
	      return 9;
	    }
	  if (c == d + 20)
	    break;
	  return 8;
	case 24:
	  do
	    {
	      if (c == d)
		break;
	      c--;
	    }
	  while (c >= 10);
	  return 19;
	default:
	  for (int e = 0; e < c; ++e)
	    if (e == d)
	      break;
	  return 0;
	}
      break;
    default:
      return 0;
    case 9:
      return 17;
    }
}	// { dg-warning "control reaches end of non-void function" }

template <int N>
int
f3 (int a, int b, int c)
{
  C f;
  switch (a)
    {
    case 0:
      switch (b)
        {
	case 13:
	  return 7;
	case 24:
	  if (c == 5)
	    break;
	  return 19;
	default:
	  return 0;
	}
      break;
    default:
      return 0;
    case 9:
      return 17;
    }
}	// { dg-warning "control reaches end of non-void function" }

template <int N>
int
f4 (int a, int b, int c, int d)
{
  C f;
  switch (a)
    {
    case 0:
      switch (b)
        {
	case 13:
	  while (c >= 10)
	    {
	      if (c == d)
		break;
	      c--;
	    }
	  return 7;
	case 29:
	  switch (d)
	    {
	    case 35:
	      break;
	    default:
	      return 9;
	    }
	  if (c == d + 20)
	    break;
	  return 8;
	case 24:
	  do
	    {
	      if (c == d)
		break;
	      c--;
	    }
	  while (c >= 10);
	  return 19;
	default:
	  for (int e = 0; e < c; ++e)
	    if (e == d)
	      break;
	  return 0;
	}
      break;
    default:
      return 0;
    case 9:
      return 17;
    }
}	// { dg-warning "control reaches end of non-void function" }

int
f5 (int a, int b, int c)
{
  return f3 <0> (a, b, c);
}

int
f6 (int a, int b, int c, int d)
{
  return f4 <2> (a, b, c, d);
}
