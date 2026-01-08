/* Test the code generation for the new __builtin_counted_by_ref
   for pointers.  */
/* { dg-do run } */
/* { dg-options "-O2" } */
#include <stdio.h>

struct annotated {
  int *c __attribute ((counted_by (b)));
  char b;
} *p_annotated; 

struct flex {
  int *c;
  short b;
} *p_flex;

struct nested_annotated {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  char *c __attribute__ ((counted_by (b)));
} *p_nested_annotated;

struct nested_flex {
  struct {
    union {
      unsigned int b;
      float f;
    };
    int n;
  };
  char *c;
} *p_nested_flex;

#define MY_ALLOC(P, PA, COUNT) ({ \
  __auto_type __p = &(P); \
  __auto_type __c = (COUNT); \
  size_t __size_1 = (sizeof (*(*__p))); \
  size_t __size_2 = (sizeof (*((*__p)->PA)) * __c); \
  if ((*__p = __builtin_malloc (__size_1))) { \
    __builtin_memset(*__p, 0, __size_1); \
    (*__p)->PA = __builtin_malloc (__size_2); \
    __auto_type ret = __builtin_counted_by_ref((*__p)->PA); \
    *_Generic(ret, void *: &(size_t){0}, default: ret) = __c; \
    if (sizeof (__builtin_counted_by_ref ((*__p)->PA)) != sizeof (char *)) \
      __builtin_abort (); \
  } \
})

int count;

int main(int argc, char *argv[])
{
  MY_ALLOC(p_annotated, c, 10);
  if (p_annotated->b != 10)
    __builtin_abort ();
  if (__alignof (*__builtin_counted_by_ref (p_annotated->c))
      != __alignof (p_annotated->b))
    __builtin_abort ();
  if (!__builtin_types_compatible_p 
	(__typeof (*__builtin_counted_by_ref (p_annotated->c)),
	 __typeof (p_annotated->b)))
    __builtin_abort ();
  if (!__builtin_types_compatible_p
	(__typeof (char[__builtin_counted_by_ref (p_annotated->c)
			== &p_annotated->b ? 1 : 10]),
	 __typeof (char[1])))
    __builtin_abort ();

  MY_ALLOC(p_flex, c, 20);
  if (p_flex->b == 20)
    __builtin_abort ();
  if (!__builtin_types_compatible_p
	(__typeof (char[__builtin_counted_by_ref (p_flex->c)
			== &p_flex->b ? 1 : 10]),
	 __typeof (char[10])))
    __builtin_abort ();

  MY_ALLOC(p_nested_annotated, c, 30);
  if (p_nested_annotated->b != 30)
    __builtin_abort ();
  if (__alignof (*__builtin_counted_by_ref (p_nested_annotated->c))
      != __alignof (p_nested_annotated->b))
    __builtin_abort ();
  if (!__builtin_types_compatible_p
	(__typeof (*__builtin_counted_by_ref (p_nested_annotated->c)),
	 __typeof (p_nested_annotated->b)))
    __builtin_abort ();

  MY_ALLOC(p_nested_flex, c, 40);
  if (p_nested_flex->b == 40)
    __builtin_abort ();

  count = p_annotated->b * 2 + p_nested_annotated->b * 3;
  struct annotated * annotated_p;
  struct flex * flex_p;
  struct nested_annotated * nested_annotated_p;
  struct nested_flex * nested_flex_p;  

  MY_ALLOC(annotated_p, c, count);
  if (annotated_p->b != count)
    __builtin_abort ();
  if (__alignof (*__builtin_counted_by_ref (annotated_p->c))
      != __alignof (annotated_p->b))
    __builtin_abort ();
  if (!__builtin_types_compatible_p
	(__typeof (*__builtin_counted_by_ref (annotated_p->c)),
	 __typeof (annotated_p->b)))
    __builtin_abort ();

  MY_ALLOC(flex_p, c, count * 2);
  if (flex_p->b == count * 2)
    __builtin_abort ();

  MY_ALLOC(nested_annotated_p, c, count * 3);
  if (nested_annotated_p->b != count * 3)
    __builtin_abort ();
  if (__alignof (*__builtin_counted_by_ref (nested_annotated_p->c))
      != __alignof (nested_annotated_p->b))
    __builtin_abort ();
  if (!__builtin_types_compatible_p
	(__typeof (*__builtin_counted_by_ref (nested_annotated_p->c)),
	 __typeof (nested_annotated_p->b)))
    __builtin_abort ();

  MY_ALLOC(nested_flex_p, c, count * 4);
  if (nested_flex_p->b == count * 4)
    __builtin_abort ();

  return 0;
}
