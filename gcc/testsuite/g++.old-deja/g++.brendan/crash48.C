// { dg-do compile }
// GROUPS passed old-abort
class internal { // { dg-error "internal::internal" }
	int field;
	int anotherfield;
};

class bug { // { dg-error "bug::bug" }
	internal* numbers;
	bug(int size);
};

bug::bug(int size) // { dg-error "bug::bug" }
{
  numbers = new internal(size * size);// { dg-error "no match" }
}

int
main()
{
  bug test; // { dg-error "no match" }
}
