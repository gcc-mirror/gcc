// { dg-do run  }
// GROUPS passed operator-delete
// Check that using the delete operator with a null pointer
// is allowed (as called for by The Book, pg. 259)

extern "C" int printf (const char *, ...); 

struct base {
	int member;
};

base* bp;

void test ()
{
	delete bp;
}

int main ()
{
	bp = (base *) 0;
	test ();

	printf ("PASS\n");
	return 0;
}
