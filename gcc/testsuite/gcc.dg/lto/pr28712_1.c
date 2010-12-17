/* PR c/28712 */
struct A;

extern struct A *a;

struct A { } __attribute__((packed));

struct B __attribute__((aligned (sizeof (int))));

extern struct B *b;

struct B { int i; } __attribute__((packed));
