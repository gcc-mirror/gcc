/*
REQUIRED_ARGS: -checkaction=context
TEST_OUTPUT:
---
fail_compilation/dassert.d(14): Error: expression `AliasSeq!(0, 0)` of type `(int, int)` does not have a boolean value
fail_compilation/dassert.d(21): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/dassert.d(29): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/dassert.d(40): Error: expression `issue()` of type `void` does not have a boolean value
---
*/
#line 10

struct Baguette { int bread, floor; }
void main ()
{
    assert(Baguette.init.tupleof);
}

// https://issues.dlang.org/show_bug.cgi?id=21590
void issue21590()
{
   int a, b = 1;
   assert (a = b);

   static ref int get()
   {
	   static int i;
	   return i;
   }

	assert(get() = 1);

	// No errors for binary assignments (regardless of -checkaction=context)
	int[] arr;
	assert(arr ~= 1);
	assert(a += b);
}

// https://issues.dlang.org/show_bug.cgi?id=21798
void issue()
{
	assert(issue());
}
