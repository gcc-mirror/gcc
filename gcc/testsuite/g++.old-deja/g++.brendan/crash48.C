// Build don't link: 
// GROUPS passed old-abort
class internal { // ERROR - candidates are
	int field;
	int anotherfield;
};

class bug { // ERROR - several errors
	internal* numbers;
	bug(int size);
};

bug::bug(int size)
{ // ERROR - candidates
	numbers = new internal(size * size);// ERROR -  no match.*
}

int
main()
{
	bug test;// ERROR -  no match
}
