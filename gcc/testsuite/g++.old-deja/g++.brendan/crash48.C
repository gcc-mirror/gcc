// { dg-do assemble  }
// GROUPS passed old-abort
class internal { // { dg-error "" } candidates are
	int field;
	int anotherfield;
};

class bug { // { dg-error "" } several errors
	internal* numbers;
	bug(int size);
};

bug::bug(int size)
{ // { dg-error "" } candidates
	numbers = new internal(size * size);// { dg-error "" }  no match.*
}

int
main()
{
	bug test;// { dg-error "" }  no match
}
