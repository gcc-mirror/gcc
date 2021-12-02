// https://issues.dlang.org/show_bug.cgi?id=19179

struct SmallStruct { int x, y; };

SmallStruct test_small(SmallStruct);
void test_small_noret(SmallStruct);

void cppmain()
{
	SmallStruct s;
	s.x = 10;
	s.y = 20;
	test_small(s);
	test_small_noret(s);
}
