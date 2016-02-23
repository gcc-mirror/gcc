/* { dg-options "-Wunused-variable" } */

/* Verify that ignoring -Wunused-variable works, for various placements
   of the variable and the _Pragma.  */

/* Test 1: the _Pragma is in a macro, but the affected code isn't.  */

#pragma GCC diagnostic push

#define MACRO_1 \
    _Pragma("GCC diagnostic ignored \"-Wunused-variable\"")

int test_1()
{
    _Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
    int x;
    return 0;
}
#pragma GCC diagnostic pop


/* Test 2: neither the _Pragma nor the affected code are in a macro.  */

#pragma GCC diagnostic push
int test_2()
{
    _Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
    int x;
    return 0;
}
#pragma GCC diagnostic pop


/* Test 3: the _Pragma isn't in a macro, but the affected code is.  */

#define MACRO_3 \
    int x;

#pragma GCC diagnostic push
int test_3()
{
    _Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
    MACRO_3
    return 0;
}
#pragma GCC diagnostic pop


/* Test 4: the _Pragma and the affected code are in different macros.  */

#pragma GCC diagnostic push
#define MACRO_4A \
    _Pragma("GCC diagnostic ignored \"-Wunused-variable\"")

#define MACRO_4B \
    int x;

int test_4()
{
    MACRO_4A;
    MACRO_4B
    return 0;
}
#pragma GCC diagnostic pop


/* Test 5: both the _Pragma and the affected code are in the same macro.  */

#pragma GCC diagnostic push
#define MACRO_5 \
    _Pragma("GCC diagnostic ignored \"-Wunused-variable\"") \
    int x;

int test_5()
{
    MACRO_5;
    return 0;
}
#pragma GCC diagnostic pop
