// Build don't link: 
// GROUPS passed initialization
struct myChoiceList
{
	int bla;
	int blubb;
	int brummbrumm;
};

extern const myChoiceList foo;

extern const myChoiceList foo = {1,1,1};

// finish_decl should have an exclusion so an error is not produced
// for this line.
extern const myChoiceList foo;
