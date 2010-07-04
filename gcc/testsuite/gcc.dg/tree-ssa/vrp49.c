/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void) __attribute__((noreturn));
int n;
float *x;
int main()
{
  if (n > 0)
    {
      int i = 0;
      do
	{
	  long long index;
	  i = i + 1;
	  index = i;
	  if (index <= 0)
	    link_error ();
	  x[index] = 0;
	  i = i + 1;
	  index = i;
	  if (index <= 0)
	    link_error ();
	  x[index] = 0;
	}
      while (i < n);
    }
}
