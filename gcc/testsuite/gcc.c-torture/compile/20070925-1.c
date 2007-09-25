/* PR tree-optimization/32694 */

typedef signed long long int WordS64;
typedef unsigned long long int Word64;

int
foo (Word64 * p)
{
  while (1)
    {
      WordS64 c = 0x1llu;
      WordS64 x = *p;
      if (c >= 0)
	{
	  if (x > (WordS64) 0x7FFFFFFFFFFFFFFFll - c)
	    return 6;
	}
      else if (x < (WordS64) 0x8000000000000000ll - c)
	return 7;
      p++;
    }
}
