// Build don't link: 
// GROUPS passed old-abort
class internal {
	int field;
	int anotherfield;
}; // ERROR - candidates are

class bug {
	internal* numbers;
	bug(int size);
}; // ERROR - several errors

bug::bug(int size)
{ // ERROR - candidates
	numbers = new internal(size * size);// ERROR -  no match.*
}

int
main()
{
	bug test;// ERROR -  no match
}
