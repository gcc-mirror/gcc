/* PR target/52991 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

#define CHECK(expr) extern char c[(expr) ? 1 : -1]
#define offsetof(x, y) __builtin_offsetof (x, y)

struct test_sp1 {
    int a;
    short b;
    int c;
    char d;
} __attribute__((packed,ms_struct));

CHECK (sizeof (struct test_sp1) == 11);
CHECK (offsetof (struct test_sp1, a) == 0);
CHECK (offsetof (struct test_sp1, b) == 4);
CHECK (offsetof (struct test_sp1, c) == 6);
CHECK (offsetof (struct test_sp1, d) == 10);

struct test_sp3 {
    int a;
    short b __attribute__((aligned(8)));
    int c;
    char d;
} __attribute__((packed,ms_struct));

CHECK (sizeof (struct test_sp3) == 16);
CHECK (offsetof (struct test_sp3, a) == 0);
CHECK (offsetof (struct test_sp3, b) == 8);
CHECK (offsetof (struct test_sp3, c) == 10);
CHECK (offsetof (struct test_sp3, d) == 14);

struct test_s4 {
    int a;
    short b;
    int c:15;
    char d;
} __attribute__((ms_struct));

CHECK (sizeof (struct test_s4) == 16);
CHECK (offsetof (struct test_s4, a) == 0);
CHECK (offsetof (struct test_s4, b) == 4);
CHECK (offsetof (struct test_s4, d) == 12);
