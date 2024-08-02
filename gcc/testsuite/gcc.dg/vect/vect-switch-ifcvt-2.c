/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

/* Check for cases currently not supported by switch tree if conversion.  */

int
f1 (char *s)
{
  int c = 0;
  int i;
  for (i = 0; i < 64; i++)
    {
      switch (*s)
	{
	case ',':
	case '|':
	  c++;
	  break;
	case '^':
	  c += 2;
	  break;
	}
      s++;
    }
  return c;
}

int
f2 (char *s)
{
  int c = 0;
  int i;
  for (i = 0; i < 64; i++)
    {
      switch (*s)
	{
	case ',':
	case '|':
	  c++;
	  /*fallthrough*/
	default:
	  c+=2;
	}
      s++;
    }
  return c;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" } } */
