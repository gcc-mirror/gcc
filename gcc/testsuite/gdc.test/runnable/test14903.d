import core.stdc.stdio : printf;

__gshared int counter;

int getID(int expectedID) nothrow {
    printf("getID: counter = %d, expecting %d\n", counter, expectedID);
    assert(counter == expectedID);
    ++counter;
    return expectedID;
}

ref int getCounterRef(int expectedID) nothrow {
    getID(expectedID);
    return counter;
}

struct StructWithDtor {
    __gshared int numDtor;
    int id;

    this(int expectedID) nothrow {
        printf("constructing %d\n", expectedID);
        this.id = getID(expectedID);
    }

    ~this() nothrow {
        printf("destructing %d\n", id);
        ++numDtor;
    }
}

StructWithDtor make(int expectedID, bool doThrow) {
    if (doThrow)
        throw new Exception("make()");
    return StructWithDtor(expectedID);
}

void evaluationOrder(int a, int b, StructWithDtor c, int d, int e, ref int f, StructWithDtor g, int h, int i) {
    assert(f is counter);
}
void evaluationOrderTest() {
    counter = StructWithDtor.numDtor = 0;
    evaluationOrder(getID(0), getID(1), StructWithDtor(2), getID(3), getID(4), getCounterRef(5), make(6, false), getID(7), getID(8));
    assert(counter == 9);

    // TODO: add right-to-left test (array ops)
}

void dtors(StructWithDtor a, StructWithDtor b, StructWithDtor c, StructWithDtor d) {
    throw new Exception("dtors()");
}
void dtorsTest() {
    // no throw in args, but in callee
    counter = StructWithDtor.numDtor = 0;
    try {
        dtors(StructWithDtor(0), make(1, false), StructWithDtor(2), make(3, false));
        assert(0);
    } catch (Exception) {}
    assert(counter == 4);
    assert(StructWithDtor.numDtor == 4);

    // throw in last arg
    counter = StructWithDtor.numDtor = 0;
    try {
        dtors(StructWithDtor(0), make(1, false), StructWithDtor(2), make(3, true));
        assert(0);
    } catch (Exception) {}
    assert(counter == 3);
    assert(StructWithDtor.numDtor == 3);

    // throw in 2nd arg
    counter = StructWithDtor.numDtor = 0;
    try {
        dtors(StructWithDtor(0), make(1, true), StructWithDtor(2), make(3, true));
        assert(0);
    } catch (Exception) {}
    assert(counter == 1);
    assert(StructWithDtor.numDtor == 1);

    // TODO: test exception chaining with throwing dtors
}

void main() {
    evaluationOrderTest();
    dtorsTest();
}
