// { dg-do assemble  }

enum tristate { no = -1, maybe, yes };

void foobar ()
{
  tristate var = no;		// { dg-bogus "" } 
}
