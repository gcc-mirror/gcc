// Build don't link: 
// GROUPS passed errors
void f( int a) {
  int a;	// this should be an error now// ERROR - .*
	{
		int a;
	}
};
