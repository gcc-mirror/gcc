// Build don't link: 
// GROUPS passed bit-fields
	struct {
	    char c;
	    int i:8;
	} s;
	
	main()
	{
	    int &ir = s.i;
	    int *ip = &s.i;// ERROR - .* , XFAIL *-*-*
	    ir = 10;
	}
