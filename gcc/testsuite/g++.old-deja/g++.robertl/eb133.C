// { dg-do assemble  }
// Gives ICE 109
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de> 
// Reported against EGCS snaps 98/06/28.

// { dg-error "forward declaration" "" { target *-*-* } 0 }

using namespace std;

int main()
{
	try {
	}
	catch (bad_alloc) { // { dg-error "invalid use" }
		return 1;
	}
	return 0;
}

