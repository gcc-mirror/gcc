// Verify that there are no spurious warnings in nested switch statements due
// to the unnecessary break in the inner switch block.
// { dg-do compile }
// { dg-options "-Wimplicit-fallthrough" } */

int
foo (int c1, int c2, int c3)
{
  switch (c2)
    {
    case 0:   
      switch (c3)	// { dg-bogus "may fall through" }
	{
	case 0:
	  if (c1)
	    return 1;
	  else
	    return 2;
	  break;

	default:
	  return 3;
	}

    case 1: 
      return 4;
    default:
      return 5;
      break;
    }
}
