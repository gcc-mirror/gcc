// { dg-do compile }
// { dg-options "-O2" }

void baz (int *, int *);

#define MEMCPY(d,s,l) __builtin___memcpy_chk (d, s, l, __builtin_object_size (d, 0))

int
foo ()
{
  int *p = new int;
  int *q = new int[4];
  MEMCPY (p, "abcdefghijklmnopqrstuvwxyz", sizeof (int));
  MEMCPY (q, "abcdefghijklmnopqrstuvwxyz", 4 * sizeof (int));
  baz (p, q);
}

int
bar ()
{
  int *p = new int;
  int *q = new int[4];
  MEMCPY (p, "abcdefghijklmnopqrstuvwxyz", sizeof (int) + 1);		// { dg-warning "will always overflow destination buffer" }
  MEMCPY (q, "abcdefghijklmnopqrstuvwxyz", 4 * sizeof (int) + 1);	// { dg-warning "will always overflow destination buffer" }
  baz (p, q);
}
