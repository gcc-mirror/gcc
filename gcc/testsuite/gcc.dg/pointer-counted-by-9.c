/* Test the code generation for the new attribute counted_by.
   And also the offsetof operator on such array.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-original" } */

#include <stdlib.h>

typedef __UINTPTR_TYPE__ uintptr_t;

struct annotated {
  int b;
  char *c __attribute__ ((counted_by (b)));
} *p_annotated;

struct flex {
  int b;
  char *c; 
}; 

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
      int b;
      float f;	
    };
    int n;
  };
  char *c;
};

void __attribute__((__noinline__)) setup (int normal_count, int attr_count)
{
  p_annotated
    = (struct annotated *)malloc (sizeof (struct annotated));
 
  p_annotated->c = (char *) malloc (sizeof (char) * attr_count); 
  p_annotated->b = attr_count;

  p_nested_annotated
    = (struct nested_annotated *)malloc (sizeof (struct nested_annotated));
  p_nested_annotated->c = (char *) malloc (attr_count *  sizeof (char));
  p_nested_annotated->b = attr_count;

  return;
}

void __attribute__((__noinline__)) test (char a, char b)
{
  if (__builtin_offsetof (struct annotated, c)
      != __builtin_offsetof (struct flex, c))
    abort ();
  if (__builtin_offsetof (struct nested_annotated, c) 
      != __builtin_offsetof (struct nested_flex, c)) 
    abort ();

  if (__alignof (*p_annotated->c) != __alignof (char))
    abort ();
  if (__alignof (*p_nested_annotated->c) != __alignof (char))
    abort ();

  p_annotated->c[2] = a;
  p_nested_annotated->c[3] = b;
}

int main(int argc, char *argv[])
{
  setup (10,10);   
  test ('A', 'B');
  if (p_annotated->c[2] != 'A') abort ();
  if (p_nested_annotated->c[3] != 'B') abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "ACCESS_WITH_SIZE" 4 "original" } } */
