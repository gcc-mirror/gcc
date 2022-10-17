/*
TEST_OUTPUT:
---
fail_compilation/fail11151.d(30): Error: overlapping initialization for field `a` and `y`
---
*/

//extern(C) int printf(const char*, ...);

union U
{
    struct
    {
        align(1) long a;
        align(1) int b;
    }
    struct
    {
        align(1) int x;
        align(1) long y;
    }
}
void main()
{
    static assert(U.a.offsetof == 0);
    static assert(U.b.offsetof == 8);
    static assert(U.x.offsetof == 0);
    static assert(U.y.offsetof == 4);

    U u = {a:1, y:2};   // overlapped initializing U.a and U.y

    //printf("u.a = %lld\n", u.a);    // 8589934593 , Wrong!
    //printf("u.b = %d\n",   u.b);    // 0
    //printf("u.x = %d\n",   u.x);    // 1
    //printf("u.y = %lld\n", u.y);    // 2
}
