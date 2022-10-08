/* When -fstrict-flex-arrays=1, [], [0], and [1] are treated as flexible
   arrays.  */
/* PR tree-optimization/101836 */
/* { dg-do run } */
/* { dg-options "-O2 -fstrict-flex-arrays=1" } */

#include <stdio.h>

#define expect(p, _v) do { \
    size_t v = _v; \
    if (p == v) \
        printf("ok:  %s == %zd\n", #p, p); \
    else \
	{  \
          printf("WAT: %s == %zd (expected %zd)\n", #p, p, v); \
	  __builtin_abort (); \
	} \
} while (0);

struct trailing_array_1 {
    int a;
    int b;
    int c[4];
};

struct trailing_array_2 {
    int a;
    int b;
    int c[1];
};

struct trailing_array_3 {
    int a;
    int b;
    int c[0];
};
struct trailing_array_4 {
    int a;
    int b;
    int c[];
};

void __attribute__((__noinline__)) stuff(
    struct trailing_array_1 *normal,
    struct trailing_array_2 *trailing_1,
    struct trailing_array_3 *trailing_0,
    struct trailing_array_4 *trailing_flex)
{
    expect(__builtin_object_size(normal->c, 1), 16);
    expect(__builtin_object_size(trailing_1->c, 1), -1);
    expect(__builtin_object_size(trailing_0->c, 1), -1);
    expect(__builtin_object_size(trailing_flex->c, 1), -1);
}

int main(int argc, char *argv[])
{
    stuff((void *)argv[0], (void *)argv[0], (void *)argv[0], (void *)argv[0]);

    return 0;
}
