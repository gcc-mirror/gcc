// { dg-do compile }
// GROUPS passed old-abort
class internal { // { dg-message "internal::internal|candidate expects|no known conversion" }
	int field;
	int anotherfield;
};

class bug { // { dg-message "bug::bug|candidate expects" }
	internal* numbers;
	bug(int size);
};

bug::bug(int size) // { dg-message "bug::bug|candidate expects" }
{
  numbers = new internal(size * size);// { dg-error "no match" }
}

int
main()
{
  bug test; // { dg-error "no match" }
}
