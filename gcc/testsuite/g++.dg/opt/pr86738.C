// PR c++/86738
// { dg-do compile }

struct S { int s; };
unsigned char a[20];
unsigned char *p = &a[(__UINTPTR_TYPE__) &((S *) 0)->s];

void
foo ()
{
  __builtin_memcpy (&a[15], &a[(__UINTPTR_TYPE__) &((S *) 0)->s], 2);
}
