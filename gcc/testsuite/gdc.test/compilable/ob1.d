// REQUIRED_ARGS: -preview=dip1021

/* Should compile successfully
 */


struct Allocation {
    int* ptr;
    size_t length;
}

void canFind(scope Allocation);

int* malloc();
void free(int*);
void pitcher();
void borrow(scope int*);
void borrow2c(const scope int*, const scope int*);
void out1(out int*);


/*****************************/

@live int* foo1(int* p)
{
    return p;   // consumes owner
}

@live int* foo2()
{
    int* p = null;
    return p;      // consumes owner
}

@live int* foo3(int* p)
{
    scope int* q = p;  // borrows from p
    return p;          // use of p ends borrow in q
}

@live int* foo4(int* p)
{
    scope int* bq = p;          // borrow
    scope const int* cq = p;    // const borrow
    return p;                   // ends both borrows
}

/*******************************/

@live void foo5()
{
    auto p = malloc();
    scope(exit) free(p);
    pitcher();
}

/*******************************/

void deallocate(int* ptr, size_t length) @live
{
    canFind(Allocation(ptr, length)); // canFind() borrows ptr
    free(ptr);
}


/*******************************/


@live int* test1()
{
    auto p = malloc();
    scope b = p;
    return p;
}

@live int* test2()
{
    auto p = malloc();
    auto q = p;
    return q;
}

@live void test3()
{
    auto p = malloc();
    free(p);
}

@live void test4()
{
    auto p = malloc();
    borrow(p);
    free(p);
}

@live void test5()
{
    auto p = malloc();
    scope q = p;
    borrow2c(p, p);
    free(p);
}

@live void test6()
{
    int* p = void;
    out1(p);  // initialize
    free(p);  // consume
}


/*******************************/

void zoo1(int);

@live void zoo2() {
    int* p = malloc();
    zoo1(*p);  // does not consume p
    free(p);
}

@live void zoo3() {
    int** p = cast(int**)malloc();
    free(*p); // consumes p
}

@live void zoo4() {
    int[] a = malloc()[0 .. 1];
    zoo1(a[0]);  // does not consume a
    free(a.ptr); // consumes a
}

@live void zoo5() {
    int*[] a = (cast(int**)malloc())[0 .. 1];
    free(a[0]); // consumes a
}

struct S { int i; int* p; }

@live void zoo6() {
    S* s = cast(S*)malloc();
    zoo1(s.i);    // does not consume s
    free(cast(int*)s);
}

@live void zoo7() {
    S* s = cast(S*)malloc();
    free(s.p);    // consumes s
}

/*******************************
 * https://issues.dlang.org/show_bug.cgi?id=21854
 */

@live void test21854()
{
    foreach(int tmp; 0..10) { }

    int key = 0;
    int limit = 10;
    for (; key < limit; key += 1)
    {
        int tmp = key;
    }
}
