// This test failed with GGC_ALWAYS_COLLECT because not all unparsed
// inline methods were registered with GC.
// { dg-do compile }

const char *foo ()
{
  struct A
  {
    const char *a1 ()
    {
      return "a1";
    }
    const char *a2 ()
    {
      struct B
      {
	const char *b1 ()
	{
	  return "b1";
	}
	const char *b2 ()
	{
	  struct C
	  {
	    const char *c1 ()
	    {
	      return "c1";
	    }
	    const char *c2 ()
	    {
	      return "c2";
	    }
	  };
	  return "b2";
	}
	const char *b3 ()
	{
	  return "b3";
	}
      };
      return "a2";
    }
    const char *a3 ()
    {
      return "a3";
    }
  };
  return "foo";
}
