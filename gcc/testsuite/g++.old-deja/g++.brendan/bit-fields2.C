// Build don't link: 
// GROUPS passed bit-fields
	struct {
	    char c;
	    int i:8;
	} s;
	
	int main()
	{
	    int &ir = s.i;	// ERROR - address of bitfield
	    int *ip = &s.i;	// ERROR - address of bitfield
	    ir = 10;
	}
