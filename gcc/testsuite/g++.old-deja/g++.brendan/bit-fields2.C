// { dg-do assemble  }
// GROUPS passed bit-fields
	struct {
	    char c;
	    int i:8;
	} s;
	
	int main()
	{
	    int &ir = s.i;	// { dg-error "" } address of bitfield
	    int *ip = &s.i;	// { dg-error "" } address of bitfield
	    ir = 10;
	}
