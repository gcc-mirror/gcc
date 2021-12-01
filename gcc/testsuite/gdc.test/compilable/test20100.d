// REQUIRED_ARGS: -checkaction=context
struct STuple {
	bool opEquals(STuple) { return false; }
}

class CTuple {
}

void testStruct() {
	STuple t1;
	assert(t1 == t1);
}

void testClass() {
	CTuple t1 = new CTuple();
	assert(t1 == t1);
}

// https://issues.dlang.org/show_bug.cgi?id=20331
void testAnonymousFunction()
{
    bool function() check = () => true;
    assert(check());

    bool result = true;
    assert((() => result)());
}

void main() {
    testStruct();
    testClass();
    testAnonymousFunction();
}

// https://issues.dlang.org/show_bug.cgi?id=20989
 void test20989() @safe
{
    uint[] arr = [1, 2, 3];
    assert(arr.ptr);
    assert(!arr.ptr);
    assert(arr.ptr is arr.ptr);
}

// https://issues.dlang.org/show_bug.cgi?id=21765
ref int func21765(int);
void test21765()
{
    assert((func21765(1) = 2) == 2);
    assert((1.func21765 = 2) == 2);
}
