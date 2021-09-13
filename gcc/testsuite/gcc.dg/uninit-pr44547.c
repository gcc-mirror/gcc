/* PR tree-optimization/44547 - -Wuninitialized reports false warning
   in nested switch statements
   { dg-do compile }
   { dg-options "-O1 -Wall" } */

__attribute__ ((noipa)) int test_O1 (int argc)
{
  switch( argc )
    {
    case 1:
    case 2:
    case 4:
      {
	int n;
	switch( argc )
	  {
	  case 1:
	  case 2:
	  case 4:
	    n = argc;
	    break;
	  }

	return n;

	break;
      }
    }

  return 0;
}


#pragma GCC optimize ("2")

__attribute__ ((noipa)) int test_O2 (int argc)
{
  switch( argc )
    {
    case 1:
    case 2:
    case 4:
      {
	int n;
	switch( argc )
	  {
	  case 1:
	  case 2:
	  case 4:
	    n = argc;
	    break;
	  }

	return n;

	break;
      }
    }

  return 0;
}
