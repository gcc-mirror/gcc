/* A reminder to process ops in generate_expr_as_of_bb exactly once.  */

long __attribute__((noinline))
foo (long ct, long cf, _Bool p1, _Bool p2, _Bool p3)
{
  long diff;

  diff = ct - cf;

  if (p1)
    {
      if (p2)
	{
	  if (p3)
	    {
	      long tmp = ct;
	      ct = cf;
	      cf = tmp;
	    }
	  diff = ct - cf;
	}

      return diff;
    }

  abort ();
}

int main ()
{
  if (foo(2, 3, 1, 1, 1) == 0)
    abort ();
  return 0;
}
