/* Test the attribute counted_by and its usage in
__builtin_dynamic_object_size: what's the correct behavior when the
allocation size mismatched with the value of counted_by attribute?
We should always use the latest value that is hold by the counted_by
field.  */
/* { dg-do run } */
/* { dg-options "-O -fstrict-flex-arrays=3" } */

#include "builtin-object-size-common.h"

struct annotated {
  size_t foo;
  char others;
  char array[] __attribute__((counted_by (foo)));
};

#define noinline __attribute__((__noinline__))
#define SIZE_BUMP 10 
#define MAX(a, b) ((a) > (b) ? (a) : (b))

/* In general, Due to type casting, the type for the pointee of a pointer
   does not say anything about the object it points to,
   So, __builtin_object_size can not directly use the type of the pointee
   to decide the size of the object the pointer points to.

   There are only two reliable ways:
   A. observed allocations  (call to the allocation functions in the routine)
   B. observed accesses     (read or write access to the location of the
                             pointer points to)

   That provide information about the type/existence of an object at
   the corresponding address.

   For A, we use the "alloc_size" attribute for the corresponding allocation
   functions to determine the object size;
   (We treat counted_by attribute the same as the "alloc_size" attribute)

   For B, we use the SIZE info of the TYPE attached to the corresponding access.

   The only other way in C which ensures that a pointer actually points
   to an object of the correct type is 'static':

   void foo(struct P *p[static 1]);

   See https://gcc.gnu.org/pipermail/gcc-patches/2023-July/624814.html
   for more details.  */

/* In the following function, malloc allocated more space than the value
   of counted_by attribute.  Then what's the correct behavior we expect 
   the __builtin_dynamic_object_size should have for each of the cases?  */

static struct annotated * noinline alloc_buf_more (size_t index)
{
  struct annotated *p;
  size_t allocated_size
    = MAX (sizeof (struct annotated),
	   (__builtin_offsetof (struct annotated, array[0])
	    + (index + SIZE_BUMP) * sizeof (char)));
  p = (struct annotated *) malloc (allocated_size);

  p->foo = index;

  /* When checking the observed access p->array, we have info on both
    observered allocation and observed access,
    A.1 from observed allocation: 
    	allocated_size - offsetof (struct annotated, array[0])

    A.2 from the counted-by attribute:
    	p->foo * sizeof (char)

    We always use the latest value that is hold by the counted-by field.
   */

  EXPECT(__builtin_dynamic_object_size(p->array, 0),
	 (p->foo) * sizeof(char));

  EXPECT(__builtin_dynamic_object_size(p->array, 1),
	 (p->foo) * sizeof(char));

  EXPECT(__builtin_dynamic_object_size(p->array, 2),
	 (p->foo) * sizeof(char));

  EXPECT(__builtin_dynamic_object_size(p->array, 3),
	 (p->foo) * sizeof(char));

  /* When checking the pointer p, we only have info on the observed allocation.
    So, the object size info can only been obtained from the call to malloc.
    For both MAXIMUM and MINIMUM: A = (index + SIZE_BUMP) * sizeof (char)  */
  EXPECT(__builtin_dynamic_object_size(p, 0), allocated_size);
  EXPECT(__builtin_dynamic_object_size(p, 1), allocated_size);
  EXPECT(__builtin_dynamic_object_size(p, 2), allocated_size);
  EXPECT(__builtin_dynamic_object_size(p, 3), allocated_size);
  return p;
}

/* In the following function, malloc allocated less space than the value
   of counted_by attribute.  Then what's the correct behavior we expect 
   the __builtin_dynamic_object_size should have for each of the cases?
   NOTE: this is an user error, GCC should issue warnings for such case.
   This is a seperate issue we should address later.  */

static struct annotated * noinline alloc_buf_less (size_t index)
{
  struct annotated *p;
  size_t allocated_size
    = MAX (sizeof (struct annotated),
	   (__builtin_offsetof (struct annotated, array[0])
	    + (index) * sizeof (char)));
  p = (struct annotated *) malloc (allocated_size);

  p->foo = index + SIZE_BUMP;

  /* When checking the observed access p->array, we have info on both
    observered allocation and observed access,
    A.1 from observed allocation:
    	allocated_size - offsetof (struct annotated, array[0])
    A.2 from the counted-by attribute:
    	p->foo * sizeof (char)

    We always use the latest value that is hold by the counted-by field.
   */

  EXPECT(__builtin_dynamic_object_size(p->array, 0),
	 (p->foo) * sizeof(char));

  EXPECT(__builtin_dynamic_object_size(p->array, 1),
	 (p->foo) * sizeof(char));

  EXPECT(__builtin_dynamic_object_size(p->array, 2),
	 (p->foo) * sizeof(char));

  EXPECT(__builtin_dynamic_object_size(p->array, 3),
	 (p->foo) * sizeof(char));

  /* When checking the pointer p, we only have info on the observed
    allocation. So, the object size info can only been obtained from
    the call to malloc.  */
  EXPECT(__builtin_dynamic_object_size(p, 0), allocated_size);
  EXPECT(__builtin_dynamic_object_size(p, 1), allocated_size);
  EXPECT(__builtin_dynamic_object_size(p, 2), allocated_size);
  EXPECT(__builtin_dynamic_object_size(p, 3), allocated_size);
  return p;
}

int main ()
{
  struct annotated *p, *q;
  p = alloc_buf_more (10);
  q = alloc_buf_less (10);

  /* When checking the access p->array, we only have info on the counted-by
    value.  */ 
  EXPECT(__builtin_dynamic_object_size(p->array, 0), p->foo * sizeof(char));
  EXPECT(__builtin_dynamic_object_size(p->array, 1), p->foo * sizeof(char));
  EXPECT(__builtin_dynamic_object_size(p->array, 2), p->foo * sizeof(char));
  EXPECT(__builtin_dynamic_object_size(p->array, 3), p->foo * sizeof(char));
  /* When checking the pointer p, we have no observed allocation nor observed
    access, therefore, we cannot determine the size info here.  */
  EXPECT(__builtin_dynamic_object_size(p, 0), -1);
  EXPECT(__builtin_dynamic_object_size(p, 1), -1);
  EXPECT(__builtin_dynamic_object_size(p, 2), 0);
  EXPECT(__builtin_dynamic_object_size(p, 3), 0);

  /* When checking the access p->array, we only have info on the counted-by
    value.  */ 
  EXPECT(__builtin_dynamic_object_size(q->array, 0), q->foo * sizeof(char));
  EXPECT(__builtin_dynamic_object_size(q->array, 1), q->foo * sizeof(char));
  EXPECT(__builtin_dynamic_object_size(q->array, 2), q->foo * sizeof(char));
  EXPECT(__builtin_dynamic_object_size(q->array, 3), q->foo * sizeof(char));
  /* When checking the pointer p, we have no observed allocation nor observed
    access, therefore, we cannot determine the size info here.  */
  EXPECT(__builtin_dynamic_object_size(q, 0), -1);
  EXPECT(__builtin_dynamic_object_size(q, 1), -1);
  EXPECT(__builtin_dynamic_object_size(q, 2), 0);
  EXPECT(__builtin_dynamic_object_size(q, 3), 0);

  DONE ();
}
