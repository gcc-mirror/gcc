/* Test the code generation for the new attribute counted_by.
   And also the offsetof operator on such array.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-original" } */

#include <stdlib.h>

typedef __UINTPTR_TYPE__ uintptr_t;

struct annotated {
  int b;
  char c[] __attribute__ ((counted_by (b)));
} *array_annotated;

static struct annotated static_annotated = { sizeof "hello", "hello" };
static char *y = static_annotated.c;

struct flex {
  int b;
  char c[]; 
}; 

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

static struct nested_annotated nested_static_annotated
				 = { sizeof "hello1", 0, "hello1" };
static char *nested_y = nested_static_annotated.c;

struct nested_flex {
  struct {
    union {
      int b;
      float f;	
    };
    int n;
  };
  char c[];
};

void __attribute__((__noinline__)) setup (int normal_count, int attr_count)
{
  array_annotated
    = (struct annotated *)malloc (sizeof (struct annotated)
				  + attr_count *  sizeof (char));
  array_annotated->b = attr_count;

  array_nested_annotated
    = (struct nested_annotated *)malloc (sizeof (struct nested_annotated)
					 + attr_count *  sizeof (char));
  array_nested_annotated->b = attr_count;

  return;
}

void __attribute__((__noinline__)) test (char a, char b)
{
  if (__builtin_offsetof (struct annotated, c[0])
      != __builtin_offsetof (struct flex, c[0]))
    abort ();
  if (__builtin_offsetof (struct annotated, c[1])
      != __builtin_offsetof (struct flex, c[1]))
    abort ();
  if (__builtin_offsetof (struct nested_annotated, c[0]) 
      != __builtin_offsetof (struct nested_flex, c[0])) 
    abort ();
  if (__builtin_offsetof (struct nested_annotated, c[1]) 
      != __builtin_offsetof (struct nested_flex, c[1])) 
    abort ();

  if (__builtin_types_compatible_p (typeof (array_annotated->c),
				    typeof (&(array_annotated->c)[0])))
    abort ();
  if (__builtin_types_compatible_p (typeof (array_nested_annotated->c),
				    typeof (&(array_nested_annotated->c)[0])))
    abort ();

  if (__alignof (array_annotated->c) != __alignof (char))
    abort ();
  if (__alignof (array_nested_annotated->c) != __alignof (char))
    abort ();

  if ((uintptr_t) array_annotated->c != (uintptr_t) &array_annotated->c)
    abort ();
  if ((uintptr_t) array_nested_annotated->c
       != (uintptr_t) &array_nested_annotated->c)
    abort ();

  array_annotated->c[2] = a;
  array_nested_annotated->c[3] = b;

  if (y[2] != 'l') abort ();
  if (nested_y[4] !='o') abort ();

}

int main(int argc, char *argv[])
{
  setup (10,10);   
  test ('A', 'B');
  if (array_annotated->c[2] != 'A') abort ();
  if (array_nested_annotated->c[3] != 'B') abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "ACCESS_WITH_SIZE" 8 "original" } } */
