// Test to verify that the strlen() optimization doesn't make assumptions
// about the static type of the object pointed to by its argument.  See
// the following thread for background:
//   https://gcc.gnu.org/ml/gcc-patches/2018-08/msg00260.html

// { dg-do run }
// { dg-options "-O2 -Wall -fdump-tree-optimized" }

typedef __SIZE_TYPE__ size_t;

void *operator new[] (size_t, void *p) { return p; }

struct S { int x; char a[1]; char b[64]; };

__attribute__ ((noipa)) void
init (char *s)
{
  *s++ = '1';
  *s++ = '\0';
}

__attribute__ ((noipa)) void
test_dynamic_type (S *p)
{
  // The placement new call below isn't strictly valid because it
  // creates an object that is larger than the space of the p->a
  // subobject in which it is created.  However, the corresponding
  // GIMPLE considers it valid and there's apparently no way to
  // distinguish invalid cases from ones like it that might be valid.
  // If/when GIMPLE changes to make this possible this test can be
  // removed.
  char *q = new (p->a) char [16];   // { dg-warning "\\\[-Wplacement-new" }

  init (q);

  if (0 == __builtin_strlen (q))
    __builtin_abort();
}

int main ()
{
  test_dynamic_type (new S);
}
