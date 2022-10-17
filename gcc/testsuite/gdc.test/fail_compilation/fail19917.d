/*
TEST_OUTPUT:
---
fail_compilation/fail19917.d(22): Error: overlapping default initialization for field `c` and `a`
fail_compilation/fail19917.d(22): Error: overlapping default initialization for field `d` and `b`
fail_compilation/fail19917.d(39): Error: overlapping default initialization for field `b` and `a`
---
*/

struct S
{
    union
    {
        struct
        {
            int a = 3;
            int b = 4;
        }
   }
}

struct X
{
    union
    {
        struct
        {
            int a = 3;
            int b = 4;
        }
        struct
        {
            int c = 3;
            int d = 4;
        }
    }
}

struct Y
{
    union
    {
        struct
        {
            union { int a = 3; }
        }
        int b = 4;
    }
}
