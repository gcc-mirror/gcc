// REQUIRED_ARGS: -o-

/***************** CatExp *******************/

/*
TEST_OUTPUT:
---
fail_compilation/nogc2.d(20): Error: cannot use operator `~` in `@nogc` function `nogc2.testCat`
fail_compilation/nogc2.d(21): Error: cannot use operator `~` in `@nogc` function `nogc2.testCat`
fail_compilation/nogc2.d(22): Error: cannot use operator `~` in `@nogc` function `nogc2.testCat`
fail_compilation/nogc2.d(24): Error: cannot use operator `~` in `@nogc` function `nogc2.testCat`
fail_compilation/nogc2.d(25): Error: cannot use operator `~` in `@nogc` function `nogc2.testCat`
fail_compilation/nogc2.d(26): Error: cannot use operator `~` in `@nogc` function `nogc2.testCat`
fail_compilation/nogc2.d(27): Error: cannot use operator `~` in `@nogc` function `nogc2.testCat`
fail_compilation/nogc2.d(28): Error: cannot use operator `~` in `@nogc` function `nogc2.testCat`
---
*/
@nogc void testCat(int[] a, string s)
{
    int[] a1 = a ~ a;
    int[] a2 = a ~ 1;
    int[] a3 = 1 ~ a;

    string s1 = s ~ s;
    string s2 = s ~ "a";
    string s3 = "a" ~ s;
    string s4 = s ~ 'c';
    string s5 = 'c' ~ s;

    string s6 = "a" ~ "b";      // no error
    string s7 = "a" ~ 'c';      // no error
    string s8 = 'c' ~ "b";      // no error
}

/***************** CatAssignExp *******************/

/*
TEST_OUTPUT:
---
fail_compilation/nogc2.d(47): Error: cannot use operator `~=` in `@nogc` function `nogc2.testCatAssign`
fail_compilation/nogc2.d(49): Error: cannot use operator `~=` in `@nogc` function `nogc2.testCatAssign`
fail_compilation/nogc2.d(50): Error: cannot use operator `~=` in `@nogc` function `nogc2.testCatAssign`
---
*/
@nogc void testCatAssign(int[] a, string s)
{
    a ~= 1;

    s ~= "a";
    s ~= 'c';
}

/***************** ArrayLiteralExp *******************/

@nogc int* barA();

/*
TEST_OUTPUT:
---
fail_compilation/nogc2.d(69): Error: array literal in `@nogc` function `nogc2.testArray` may cause a GC allocation
fail_compilation/nogc2.d(70): Error: array literal in `@nogc` function `nogc2.testArray` may cause a GC allocation
---
*/
@nogc void testArray()
{
    enum arrLiteral = [null, null];

    int* p;
    auto a = [p, p, barA()];
    a = arrLiteral;
}

/***************** AssocArrayLiteralExp *******************/

/*
TEST_OUTPUT:
---
fail_compilation/nogc2.d(86): Error: associative array literal in `@nogc` function `nogc2.testAssocArray` may cause a GC allocation
fail_compilation/nogc2.d(87): Error: associative array literal in `@nogc` function `nogc2.testAssocArray` may cause a GC allocation
---
*/
@nogc void testAssocArray()
{
    enum aaLiteral = [10: 100];

    auto aa = [1:1, 2:3, 4:5];
    aa = aaLiteral;
}

/***************** IndexExp *******************/

/*
TEST_OUTPUT:
---
fail_compilation/nogc2.d(100): Error: assigning an associative array element in `@nogc` function `nogc2.testIndex` may cause a GC allocation
---
*/
@nogc void testIndex(int[int] aa)
{
    aa[1] = 0;
    int n = aa[1];
}
