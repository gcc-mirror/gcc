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
} // { dg-prune-output "-Wfree-nonheap-object" }

/* Delete of placement new.  */

void test_3 (void)
{
  char buf[sizeof(int)]; // { dg-message "region created on stack here" }
  int *p = new (buf) int (42);
  delete p; // { dg-warning "memory on the stack" }
}

// { dg-prune-output "-Wfree-nonheap-object" }

void test_4 (void)
{
  int buf[5]; // { dg-message "region created on stack here" }
  int *p = new (&buf[2]) int (42);
  delete p; // { dg-warning "memory on the stack" }
}


// { dg-prune-output "-Wfree-nonheap-object" }

void test_write_placement_after_delete (void)
{
  short *s = ::new short;
  short *lp = ::new (s) short;
  ::delete s;
  *lp = 12; /* { dg-warning "use after 'delete' of 'lp'" "write placement new after buffer deletion" } */
}

void test_read_placement_after_delete (void)
{
  short *s = ::new short;
  short *lp = ::new (s) short;
  ::delete s;
  short m = *lp; // { dg-warning "use after 'delete' of 'lp'" "read placement new after buffer deletion" }
}

struct A
{
  int x;
  int y;
};

void test_use_placement_after_destruction (void)
{
  A a;
  int *lp = ::new (&a.y) int;
  *lp = 2; /* { dg-bogus "-Wanalyzer-use-of-uninitialized-value" } */
  a.~A();
  int m = *lp; /* { dg-warning "use of uninitialized value '\\*lp'" "use of placement after the underlying buffer was destructed." } */
}

void test_initialization_through_placement (void)
{
  int x;
  int *p = ::new (&x) int;
  *p = 10;
  int z = x + 2; /* { dg-bogus "use of uninitialized value 'x'" "x has been initialized through placement pointer" } */
}

void test_partial_initialization_through_placement (void)
{
  char buf[4];
  char *p = ::new (&buf[2]) char;
  *p = 10;
  char *y = ::new (&buf[0]) char;
  char z = buf[2] + 2; /* { dg-bogus "use of uninitialized value" } */
  z = *y + 2; /* { dg-warning "use of uninitialized value '\\*y'" "y has only been partially initialized" } */
}


void test_delete_placement (void)
{
  A *a = ::new A; /* { dg-bogus "use of possibly-NULL 'operator new(8)' where non-null expected" "throwing new cannot be null" } */
  int *z = ::new (&a->y) int;
  a->~A(); // deconstruct properly
  ::operator delete (a);
  ::operator delete (z); /* { dg-warning "use after 'delete' of 'z'" }  */
}

void test_delete_placement_2 (void)
{
  A *a = ::new A; /* { dg-bogus "use of possibly-NULL 'operator new(8)' where non-null expected" "throwing new cannot be null" } */
  int *z = ::new (&a->y) int;
  delete a;
  ::operator delete (z); /* { dg-warning "use after 'delete' of 'z'" }  */
}

void test_use_placement_after_deallocation (void)
{
  A *a = ::new A ();
  int *lp = ::new (&a->y) int;
  *lp = 2; /* { dg-bogus "use of uninitialized value" } */
  ::operator delete (a);
  int m = *lp; /* { dg-warning "use after 'delete' of 'lp'" "use of placement after the underlying buffer was deallocated." } */
}
