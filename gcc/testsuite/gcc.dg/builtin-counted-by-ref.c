/* Testing the correct usage of the new __builtin_counted_by_ref.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <stdio.h>

struct annotated {
  size_t b;
  int other;
  int c[] __attribute ((counted_by (b)));
} *array_annotated;

struct flex {
  size_t b;
  int other;
  int c[];
} *array_flex;

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

extern char c_count;
extern short s_count;
extern int i_count;
extern long l_count;
extern float f_count;

extern int * foo ();

int main(int argc, char *argv[])
{
  /* The good usages.  */
  MY_ALLOC(array_annotated, c, 10);
  MY_ALLOC(array_flex, c, 20);
  MY_ALLOC(array_annotated, c, c_count);
  MY_ALLOC(array_flex, c, i_count);
  MY_ALLOC(array_annotated, c, l_count);
  MY_ALLOC(array_flex, c, c_count * 3);
  MY_ALLOC(array_annotated, c, l_count * i_count);

  /* The bad usages, issue errors.  */
  __builtin_counted_by_ref (); /* { dg-error "wrong number of arguments to" } */
  __builtin_counted_by_ref (array_annotated->c, 10); /* { dg-error "wrong number of arguments to" } */
  __builtin_counted_by_ref (array_annotated->other); /* { dg-error "must be an array" } */
  __builtin_counted_by_ref (foo());  /* { dg-error "must be an array" } */

  return 0;
}
