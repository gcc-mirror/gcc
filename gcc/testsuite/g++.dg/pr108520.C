// { dg-do compile { target c++23 } }
// { dg-options "-O2" }

static void foo () {}
struct S { void (*f) (); };

[[gnu::nonnull (1)]]
void
bar (void *x)
{
  struct S a[3] = { { foo }, { foo }, { foo } };
  for (struct S *i = a, *e = a + 3; i != e; i++)
    {
      [[assume (i->f)]];
      i->f ();
    }
}
