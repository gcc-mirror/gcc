/* { dg-additional-options "-std=gnu17 -Wno-implicit-int" } */

extern void foo (void *) __attribute__((nonnull));

void
en (jm)
{
}

void
p2 ()
{
  char *rl = 0;

  en ();
  foo (rl); /* { dg-warning "use of NULL 'rl' where non-null expected" } */
}
