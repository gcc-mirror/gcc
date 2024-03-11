/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra -fdump-tree-optimized-slim"  } */

/* This emulates what C++ trstcase pr110378-1.C looks like on 32-bit arm (or
   any architecture where the destructor returns this pointer.  It verifies
   that when it later becomes known that the return value will be removed, we
   can split a parameter even in this case.  */

struct S
{
  short move_offset_of_a;
  int *a;
};

extern int *allocate_stuff (unsigned);
extern void deallocate_stuff (void *);

static void
something_like_a_constructor (struct S *p, int len)
{
  p->a = allocate_stuff (len * sizeof (int));
  *p->a = 4;
}

static int
operation (struct S *p)
{
  return *p->a + 1;
}

static struct S * __attribute__((noinline))
something_like_an_arm32_destructor (struct S *p)
{
  deallocate_stuff (p->a);
  return p;
}

volatile int v2 = 20;

int test (void)
{
  struct S shouldnotexist;
  something_like_a_constructor (&shouldnotexist, v2);
  v2 = operation (&shouldnotexist);
  something_like_an_arm32_destructor (&shouldnotexist);
  return 0;
}

/* { dg-final { scan-ipa-dump "Will split parameter 0" "sra"  } } */
/* { dg-final { scan-tree-dump-not "shouldnotexist" "optimized" } } */
