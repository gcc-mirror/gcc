/* Testing the correct usage of the new __builtin_counted_by_ref for 
   pointers.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <stdio.h>

struct annotated {
  size_t b;
  int *c __attribute ((counted_by (b)));
  int other;
} *p_annotated;

struct flex {
  size_t b;
  int *c;
  int other;
} *p_flex;

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

extern char c_count;
extern short s_count;
extern int i_count;
extern long l_count;
extern float f_count;

extern int * foo ();

int main(int argc, char *argv[])
{
  /* The good usages.  */
  MY_ALLOC(p_annotated, c, 10);
  MY_ALLOC(p_flex, c, 20);
  MY_ALLOC(p_annotated, c, c_count);
  MY_ALLOC(p_flex, c, i_count);
  MY_ALLOC(p_annotated, c, l_count);
  MY_ALLOC(p_flex, c, c_count * 3);
  MY_ALLOC(p_annotated, c, l_count * i_count);

  /* The bad usages, issue errors.  */
  __builtin_counted_by_ref (); /* { dg-error "wrong number of arguments to" } */
  __builtin_counted_by_ref (p_annotated->c, 10); /* { dg-error "wrong number of arguments to" } */
  __builtin_counted_by_ref (p_annotated->other); /* { dg-error "must be an array or pointer" } */
  __builtin_counted_by_ref (foo());  /* { dg-error "must be a field of a structure" } */

  return 0;
}
