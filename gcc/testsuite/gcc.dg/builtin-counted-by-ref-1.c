/* Test the code generation for the new __builtin_counted_by_ref.  */
/* { dg-do run } */
/* { dg-options "-O2" } */
#include <stdio.h>

struct annotated {
  char b;
  int c[] __attribute ((counted_by (b)));
} *array_annotated; 

struct flex {
  short b;
  int c[];
} *array_flex;

struct nested_annotated {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  char c[] __attribute__ ((counted_by (b)));
} *array_nested_annotated;

struct nested_flex {
  struct {
    union {
      unsigned int b;
      float f;
    };
    int n;
  };
  char c[];
} *array_nested_flex;

#define MAX(A, B) (A > B) ? (A) : (B)
#define MY_ALLOC(P, FAM, COUNT) ({ \
  __auto_type __p = &(P); \
  __auto_type __c = (COUNT); \
  size_t __size = MAX (sizeof (*(*__p)),\
		       __builtin_offsetof (__typeof(*(*__p)),FAM) \
		       + sizeof (*((*__p)->FAM)) * __c); \
  if ((*__p = __builtin_malloc(__size))) { \
    __builtin_memset(*__p, 0, __size); \
    __auto_type ret = __builtin_counted_by_ref((*__p)->FAM); \
    *_Generic(ret, void *: &(size_t){0}, default: ret) = __c; \
    if (sizeof (__builtin_counted_by_ref ((*__p)->FAM)) != sizeof (char *)) \
      __builtin_abort (); \
  } \
})

int count;

int main(int argc, char *argv[])
{
  MY_ALLOC(array_annotated, c, 10);
  if (array_annotated->b != 10)
    __builtin_abort ();
  if (__alignof (*__builtin_counted_by_ref (array_annotated->c))
      != __alignof (array_annotated->b))
    __builtin_abort ();
  if (!__builtin_types_compatible_p 
	(__typeof (*__builtin_counted_by_ref (array_annotated->c)),
	 __typeof (array_annotated->b)))
    __builtin_abort ();
  if (!__builtin_types_compatible_p
	(__typeof (char[__builtin_counted_by_ref (array_annotated->c)
			== &array_annotated->b ? 1 : 10]),
	 __typeof (char[1])))
    __builtin_abort ();

  MY_ALLOC(array_flex, c, 20);
  if (array_flex->b == 20)
    __builtin_abort ();
  if (!__builtin_types_compatible_p
	(__typeof (char[__builtin_counted_by_ref (array_flex->c)
			== &array_flex->b ? 1 : 10]),
	 __typeof (char[10])))
    __builtin_abort ();

  MY_ALLOC(array_nested_annotated, c, 30);
  if (array_nested_annotated->b != 30)
    __builtin_abort ();
  if (__alignof (*__builtin_counted_by_ref (array_nested_annotated->c))
      != __alignof (array_nested_annotated->b))
    __builtin_abort ();
  if (!__builtin_types_compatible_p
	(__typeof (*__builtin_counted_by_ref (array_nested_annotated->c)),
	 __typeof (array_nested_annotated->b)))
    __builtin_abort ();

  MY_ALLOC(array_nested_flex, c, 40);
  if (array_nested_flex->b == 40)
    __builtin_abort ();

  count = array_annotated->b * 2 + array_nested_annotated->b * 3;
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
