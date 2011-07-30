// PR c++/49867
// { dg-options -std=c++0x }

int
main ()
{
  void (*l)();
  while (true)
    {
      switch (3)
	{
	  struct A {
	    void f()
	    {
	    case 4:		// { dg-error "case" }
	      break;		// { dg-error "break" }
	    }
	  };
	  l = []()
	    {
	    case 3:		// { dg-error "case" }
	      break;		// { dg-error "break" }
	    };
	}
    }
}
