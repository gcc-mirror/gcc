// PR c++/90947 - Simple lookup table of array of strings is miscompiled
// Verify that initializers for arrays of elements of a class type with
// "unusual" data members are correctly recognized as non-zero.
// { dg-do compile }
// { dg-options "-O1 -fdump-tree-optimized" }

struct S
{
  const char *p;
  static int i;
  enum { e };
  typedef int X;
  int: 1, b:1;
  union {
    int c;
  };
  const char *q;
};

void f (void)
{
  const struct S a[2] =
    {
     { /* .p = */ "", /* .b = */ 0, /* .c = */ 0, /* .q = */ "" },
     { /* .p = */ "", /* .b = */ 0, /* .c = */ 0, /* .q = */ "" }
    };

  if (!a[0].p || *a[0].p || !a[0].q || *a[0].q
      || !a[1].p || *a[1].p || !a[1].q || *a[1].q)
    __builtin_abort ();
}

// { dg-final { scan-tree-dump-not "abort" "optimized" } }
