/* { dg-do compile }
   { dg-options "-O0 -Wall" } */

#if __cplusplus < 201103L
# define noexcept throw ()
#endif

void* operator new (__SIZE_TYPE__, void* __p) noexcept;
void operator delete (void*, void*);

void* operator new[] (__SIZE_TYPE__, void* __p) noexcept;
void operator delete[] (void*, void*) noexcept;

struct A { A (); ~A (); int i; };

extern void *p;

void nowarn_placement_new ()
{
  char a[sizeof (A)];
  p = new (a) A ();           // { dg-bogus "-Wfree-nonheap-object" }
}


void warn_placement_new ()
{
  char a[sizeof (A)];
  p = new (a + 1) A ();       // { dg-warning "\\\[-Wplacement-new" }
                              // { dg-bogus "-Wfree-nonheap-object" "bogus" { target *-*-* } .-1 }
}


void nowarn_placement_new_array ()
{
  char a[sizeof (A)];
  p = new (a) A[1];           // { dg-bogus "-Wfree-nonheap-object" }
}


void warn_placement_new_array ()
{
  char a[sizeof (A)];
  p = new (a + 1) A[1];       // { dg-warning "\\\[-Wplacement-new" }
                              // { dg-bogus "-Wfree-nonheap-object" "bogus" { target *-*-* } .-1 }
}
