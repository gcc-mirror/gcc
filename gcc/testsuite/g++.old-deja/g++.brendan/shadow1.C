// { dg-do assemble  }
// GROUPS passed errors
void f( int a) {
  int a;	// this should be an error now// { dg-error "" } .*
	{
		int a;
	}
}
