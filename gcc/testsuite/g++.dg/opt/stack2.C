// PR c++/51060
// { dg-options "-Os -Wframe-larger-than=2000 -Werror" }

// Shows a problem of not re-using stack space:
// Compile as: g++ -c test_stack_reuse.cpp -o /dev/null -Wframe-larger-than=2048 -Werror -Os
// Result: warning: the frame size of 10240 bytes is larger than 2048 bytes [-Wframe-larger-than=]
//

struct StackObject
{
  StackObject();
  char buffer[1024];
};

void Test()
{
#define TEST_SUB() \
  StackObject();

#define TEST() \
	TEST_SUB() \
	TEST_SUB() \
	TEST_SUB() \
	TEST_SUB() \
	TEST_SUB() \
	TEST_SUB() \
	TEST_SUB() \
	TEST_SUB() \
	TEST_SUB() \
	TEST_SUB()

  TEST()
}
