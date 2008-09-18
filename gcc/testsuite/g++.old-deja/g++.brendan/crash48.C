// { dg-do compile }
// GROUPS passed old-abort
class internal { // { dg-message "internal::internal" }
	int field;
	int anotherfield;
};

class bug { // { dg-message "bug::bug" }
	internal* numbers;
	bug(int size);
};

bug::bug(int size) // { dg-message "bug::bug" }
{
  numbers = new internal(size * size);// { dg-error "no match" }
}

int
main()
{
  bug test; // { dg-error "no match" }
}
