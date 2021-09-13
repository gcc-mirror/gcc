#include "analyzer-decls.h"

int test_1 (void)
{
  int i;
  return i; /* { dg-warning "use of uninitialized value 'i'" } */
}

int test_2 (void)
{
  int i;
  return i * 2; /* { dg-warning "use of uninitialized value 'i'" } */
}

int test_3 (void)
{
  static int i;
  return i;
}

int test_4 (void)
{
  int *p;
  return *p; /* { dg-warning "use of uninitialized value 'p'" } */
}

int test_5 (int flag, int *q)
{
  int *p;
  if (flag) /* { dg-message "following 'false' branch" } */
    p = q;

  /* There should be two enodes here,
     i.e. not merging the init vs non-init states.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
  
  return *p; /* { dg-warning "use of uninitialized value 'p'" } */
}

int test_6 (int i)
{
  int arr[10];
  return arr[i]; /* { dg-warning "use of uninitialized value 'arr\\\[i\\\]'" } */
}
