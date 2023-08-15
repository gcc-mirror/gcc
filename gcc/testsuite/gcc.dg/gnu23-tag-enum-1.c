/*
 * { dg-do compile }
 * { dg-options "-std=gnu23" }
 */



void test2(void)
{
  enum ee *a;
  enum ee { F = 2 } *b;
  b = a;
}



enum A { B = 7 } y;

void g(void)
{
	// incomplete during construction
	// this is a GNU extension because enum A is used
	// before the type is completed.

	enum A { B = _Generic(&y, enum A*: 1, default: 7) };
	_Static_assert(7 == B, "");
}


