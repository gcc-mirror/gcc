#include <new>

/* Placement new.  */

void test_1 (void)
{
  char buf[sizeof(int)];
  int *p = new(buf) int (42);
}

/* Placement new[].  */

void test_2 (void)
{
  char buf[sizeof(int) * 10];
  int *p = new(buf) int[10];
}

/* Delete of placement new.  */

void test_3 (void)
{
  char buf[sizeof(int)]; // { dg-message "region created on stack here" }
  int *p = new(buf) int (42);
  delete p; // { dg-warning "memory on the stack" }
}

// { dg-prune-output "-Wfree-nonheap-object" }
