/*
REQUIRED_ARGS: -w
TEST_OUTPUT:
---
fail_compilation/test20522.d(19): Error: undefined identifier `non_existent`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20522
struct File
{
    ~this() {}
}

void main()
{
	{
		auto test = File(); // <- Essential
		non_existent;
	}
	// Warning: statement is not reachable
	string[] is_this_unreachable_question_mark;
}
