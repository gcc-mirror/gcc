// https://issues.dlang.org/show_bug.cgi?id=23789

struct __declspec(align(64)) M128A {
    char c;
};

typedef struct __declspec(align(32)) _M128B {
    int x;
} M128B, *PM128A;


void testpl(p)
struct __declspec(align(2)) S *p;
{
}

/////

struct __attribute__((aligned(64))) N128A {
    char c;
};

typedef struct __attribute__((aligned(32))) _N128B {
    int x;
} N128B, *PN128A;


void testpl2(p)
struct __attribute__((aligned(2))) S *p;
{
}
