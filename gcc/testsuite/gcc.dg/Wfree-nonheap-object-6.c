/* Similar to Wfree-nonheap-object-5.c but with attribute artificial:
   verify that warnings for the same call site from distinct callers
   include the correct function names in the inlining stack.
   { dg-do compile }
   { dg-options "-O1 -Wall" } */

struct A
{
  void *p;
};

__attribute__ ((always_inline, artificial))
inline void f0 (struct A *p)
{
  __builtin_free (p->p);      // { dg-warning "\\\[-Wfree-nonheap-object" }
}

// Expect two instances of the text below:
// { dg-regexp "In function 'f0'," "first f0 prefix" { target *-*-* } 0 }
// { dg-regexp "In function 'f0'," "second f0 prefix" { target *-*-* } 0 }

__attribute__ ((always_inline, artificial))
inline void f1 (struct A *p) { f0 (p); }
__attribute__ ((always_inline, artificial))
inline void f2 (struct A *p) { f1 (p); }

extern int array[];
// Also expect two instances of the note:
// { dg-regexp "declared here" "first note for array" { target *-*-* } .-2 }
// { dg-regexp "declared here" "second note for array" { target *-*-* } .-3 }

void foo (struct A *p)
{
  p->p = array + 1;
  f0 (p);
}

// { dg-regexp " +inlined from 'foo' at \[^:\]+Wfree-nonheap-object-6.c:35:\\d+:" "inlined from foo" }


void bar (struct A *p)
{
  p->p = array + 2;
  f2 (p);
}

// { dg-regexp " +inlined from 'f1' at \[^:\]+Wfree-nonheap-object-6.c:23:\\d+," "inlined from f1" }
// { dg-regexp " +inlined from 'f2' at \[^:\]+Wfree-nonheap-object-6.c:25:\\d+," "inlined from f2" }
// { dg-regexp " +inlined from 'bar' at \[^:\]+Wfree-nonheap-object-6.c:44:\\d+:" "inlined from bar" }
