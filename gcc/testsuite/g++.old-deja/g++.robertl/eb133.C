// Build don't link:
// Gives ICE 109
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de> 
// Reported against EGCS snaps 98/06/28.

using namespace std;

int main()
{
	try {
	}
	catch (bad_alloc) { // ERROR - parse error
		return 1;
	}
	return 0;
}

	
