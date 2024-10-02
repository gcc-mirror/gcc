/* test the combination of attribute strict_flex_array and option
   -fstrict-flex-arrays: when both attribute and option specified,
   attribute will have higher priority.  */
/* { dg-do run } */
/* { dg-options "-O2 -fstrict-flex-arrays=0" } */

#include "builtin-object-size-common.h"

#define expect(p, _v) do { \
    size_t v = _v; \
    if (p == v) \
	__builtin_printf ("ok:  %s == %zd\n", #p, p); \
    else \
	{  \
	  __builtin_printf ("WAT: %s == %zd (expected %zd)\n", #p, p, v); \
	  FAIL (); \
	} \
} while (0);

#ifdef __AVR__
/* AVR-Libc doesn't support %zd, thus use %d for size_t.  */
#undef  expect
#define expect(p, _v) do {						\
    size_t v = _v;							\
    if (p == v)								\
      __builtin_printf ("ok:  %s == %d\n", #p, p);			\
    else								\
      {									\
	__builtin_printf ("WAT: %s == %d (expected %d)\n", #p, p, v);	\
	FAIL ();							\
      }									\
} while (0);
#endif /* AVR */

struct trailing_array_1 {
    int a;
    int b;
    int c[4] __attribute__ ((strict_flex_array (1)));
};

struct trailing_array_2 {
    int a;
    int b;
    int c[1] __attribute__ ((strict_flex_array (2)));
};

struct trailing_array_3 {
    int a;
    int b;
    int c[0] __attribute__ ((strict_flex_array (3)));
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
    expect(__builtin_object_size(normal->c, 1), 4 * __SIZEOF_INT__);
    expect(__builtin_object_size(trailing_1->c, 1), __SIZEOF_INT__);
    expect(__builtin_object_size(trailing_0->c, 1), 0);
    expect(__builtin_object_size(trailing_flex->c, 1), -1);
}

int main(int argc, char *argv[])
{
    stuff((void *)argv[0], (void *)argv[0], (void *)argv[0], (void *)argv[0]);

    DONE ();
}
