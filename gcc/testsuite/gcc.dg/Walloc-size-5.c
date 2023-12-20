/* { dg-do compile } */
/* { dg-options "-Walloc-size -std=gnu11" } */

struct S { int x[10]; };
void myfree ();
void *mymalloc () __attribute__((malloc, alloc_size (16)));
void *mycalloc () __attribute__((malloc, alloc_size (16, 17)));

void
foo (void)
{
  struct S *p = mymalloc (1);
  myfree (p);
  p = mycalloc (1, 1);
  myfree (p);
  p = (struct S *) mymalloc (1);
  myfree (p);
  p = (struct S *) mycalloc (1, 1);
  myfree (p);
}
