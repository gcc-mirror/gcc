/* Test support of scalar_storage_order attribute */

/* { dg-do compile } */

struct __attribute__((scalar_storage_order("big-endian"))) S1
{
  int i;
};

struct __attribute__((scalar_storage_order("little-endian"))) S2
{
  int i;
};

extern int foo (void *);

int incompatible_call (int which, struct S1 *s1, struct S2 *s2)
{
  if (which == 1) return foo (s1); else foo (s2); /* { dg-warning "incompatible scalar storage order" } */
}

void incompatible_assign (struct S1 *s1, struct S2 *s2)
{
  void *p1, *p2;
  p1 = s1, p2 = s2; /* { dg-warning "incompatible scalar storage order" } */
}

void incompatible_init (struct S1 *s1, struct S2 *s2)
{
  void *p1 = s1, *p2 = s2; /* { dg-warning "incompatible scalar storage order" } */
}

void *incompatible_return (int which, struct S1 *s1, struct S2 *s2)
{
  if (which == 1) return s1; else return s2; /* { dg-warning "incompatible scalar storage order" } */
}
