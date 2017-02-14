// PR c++/49867
// { dg-do compile { target c++11 } }

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
	  l = []()		// { dg-warning "statement will never be executed" }
	    {
	    case 3:		// { dg-error "case" }
	      break;		// { dg-error "break" }
	    };
	}
    }
}
