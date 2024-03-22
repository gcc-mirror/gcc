// PR c++/81438
// { dg-do compile { target indirect_jumps } }
// { dg-options "" }

bool b;
int main()
{
  try
    {
      try { throw 3; }
      catch(...) {
      h:;			// { dg-warning "jump to label" }
	try { throw 7; }
	catch(...) {
	  if (b)
	    goto *&&h;		// { dg-message "computed goto" }
				// { dg-message "handled exception" "" { target *-*-* } .-1 }
	  else
	    goto *&&g;		// { dg-message "computed goto" }
				// { dg-message "handled exception" "" { target *-*-* } .-1 }
	}
      g:;			// { dg-warning "jump to label" }
	throw;
      }
    }
  catch(int v)
    {
      __builtin_printf("%d\n", v);
      if(v != 3)	       // 7 because we don't clean up the catch on
	__builtin_abort();     // computed goto
    }

  return 0;
}
