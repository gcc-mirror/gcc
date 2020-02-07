/* Verify that strncmp equalities aren't eliminated when the trailing array
   type referenced by a member pointer is smaller than the string in cases
   when the pointer pointed to by the enclosing object references an object
   sufficiently large to store a string of equal length.
  { dg-do compile }
  { dg-options "-O2 -Wall -Wextra -fdump-tree-optimized" } */

void init (void*);

struct A1 { char i, a[1]; };

void f1_arr (void)
{
  char a[9];
  init (a);

  struct A1 *p = (struct A1*)a;

  if (__builtin_strncmp (p->a, "01234567", 8) == 0)
    {
      extern void array_test (void);
      array_test ();
    }
}

void f1_ptr (void)
{
  void *p;
  init (&p);

  struct A1 *q = (struct A1*)p;

  if (__builtin_strncmp (q->a, "0123456789", 10) == 0)
    {
      extern void pointer_test (void);
      pointer_test ();
    }
}

void f1_struct (void)
{
  struct { char a[9]; } b;
  init (&b);

  struct A1 *p = (struct A1*)&b;

  if (__builtin_strncmp (p->a, "01234567", 8) == 0)
    {
      extern void struct_test (void);
      struct_test ();
    }
}

void f1_memptr (void)
{
  struct { void *p; } b;
  init (&b);

  struct A1 *p = (struct A1*)b.p;

  if (__builtin_strncmp (p->a, "0123456789", 10) == 0)
    {
      extern void memptr_test (void);
      memptr_test ();
    }
}


struct A2 { char i, a[2]; };

void f2_arr (void)
{
  char a[8];
  init (a);

  struct A2 *p = (struct A2*)a;

  if (__builtin_strncmp (p->a, "0123456", 7) == 0)
    {
      extern void array_test (void);
      array_test ();
    }
}

void f2_ptr (void)
{
  void *p;
  init (&p);

  struct A2 *q = (struct A2*)p;

  if (__builtin_strncmp (q->a, "0123456789", 10) == 0)
    {
      extern void pointer_test (void);
      pointer_test ();
    }
}

void f2_struct (void)
{
  struct { char a[8]; } b;
  init (&b);

  struct A2 *p = (struct A2*)&b;

  if (__builtin_strncmp (p->a, "0123456", 7) == 0)
    {
      extern void struct_test (void);
      struct_test ();
    }
}

void f2_memptr (void)
{
  struct { void *p; } b;
  init (&b);

  struct A2 *p = (struct A2*)b.p;

  if (__builtin_strncmp (p->a, "0123456789", 10) == 0)
    {
      extern void memptr_test (void);
      memptr_test ();
    }
}

/* { dg-final { scan-tree-dump-times "array_test" 2 "optimized" } }
   { dg-final { scan-tree-dump-times "pointer_test" 2 "optimized" } }
   { dg-final { scan-tree-dump-times "struct_test" 2 "optimized" } }
   { dg-final { scan-tree-dump-times "memptr_test" 2 "optimized" } } */
