// Yet Another testcase for signed/unsigned enums.
// Build don't link:

enum A { AA = 0, AB = 1};
enum B { BA = -1, BB = 1};

void set(int a);
void set(long a);

void
foo()
{
	set(AA);	// gets bogus error - why is this ambiguous
	set(BA);	// when this is not amibguous
}
