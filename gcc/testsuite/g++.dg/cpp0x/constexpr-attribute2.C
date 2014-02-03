// { dg-options -std=gnu++11 }

struct t { t(); };

constexpr int prio = 123;
constexpr int size = 8;
constexpr int pos = 1;
enum A { zero = 0, one, two };
__attribute__((init_priority(prio))) t a;

enum class E1 : int {
    first = 101,
    second,
    third,
};
__attribute__((init_priority(E1::second))) t b; // Should not compile?

enum E2 {
    E2_first = 141,
    E2_second,
    E2_third,
};
__attribute__((init_priority(E2_second))) t c;

void* my_calloc(unsigned, unsigned) __attribute__((alloc_size(pos,two)));
void* my_realloc(void*, unsigned) __attribute__((alloc_size(two)));

typedef char vec __attribute__((vector_size(size)));

void f(char*) __attribute__((nonnull(pos)));

void g() __attribute__((aligned(size)));
