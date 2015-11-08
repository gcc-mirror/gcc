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

struct __attribute__((scalar_storage_order("other"))) S3 { int i; }; /* { dg-error "must be one of .big-endian. or .little-endian." } */

void incompatible_assign (struct S1 *s1, struct S2 *s2)
{
  *s1 = *s2; /* { dg-error "(incompatible types|no match)" } */
}

int *addr1 (int which, struct S1 *s1, struct S2 *s2)
{
  return (which == 1 ? &s1->i : &s2->i); /* { dg-error "address of scalar with reverse storage order" } */
}

struct __attribute__((scalar_storage_order("big-endian"))) S4
{
  int a[4];
  struct S2 s2;
};

struct __attribute__((scalar_storage_order("little-endian"))) S5
{
  int a[4];
  struct S1 s1;
};

void *addr2 (int which, struct S4 *s4, struct S5 *s5)
{
  return (which == 1 ? (void *)s4->a : (void *)s5->a); /* { dg-warning "address of array with reverse scalar storage order" } */
}

void *addr3 (int which, struct S4 *s4, struct S5 *s5)
{
  return (which == 1 ? (void *)&s4->a : (void *)&s5->a); /* { dg-warning "address of array with reverse scalar storage order" } */
}

void *addr4 (int which, struct S4 *s4, struct S5 *s5)
{
  return (which == 1 ? (void *)&s4->a[0] : (void *)&s5->a[0]); /* { dg-error "address of scalar with reverse storage order" } */
}

void *addr5 (int which, struct S4 *s4, struct S5 *s5)
{
  return (which == 1 ? (void *)&s4->s2 : (void *) &s5->s1); /* ok */
}

struct __attribute__((scalar_storage_order("big-endian"))) S6
{
  int a[4][2];
  struct S2 s2[2];
};

struct __attribute__((scalar_storage_order("little-endian"))) S7
{
  int a[4][2];
  struct S1 s1[2];
};

void *addr6 (int which, struct S6 *s6, struct S7 *s7)
{
  return (which == 1 ? (void *)s6->a : (void *)s7->a); /* { dg-warning "address of array with reverse scalar storage order" } */
}

void *addr7 (int which, struct S6 *s6, struct S7 *s7)
{
  return (which == 1 ? (void *)&s6->a : (void *)&s7->a); /* { dg-warning "address of array with reverse scalar storage order" } */
}

void *addr8 (int which, struct S6 *s6, struct S7 *s7)
{
  return (which == 1 ? (void *)&s6->a[0] : (void *)&s7->a[0]); /* { dg-warning "address of array with reverse scalar storage order" } */
}

void *addr9 (int which, struct S6 *s6, struct S7 *s7)
{
  return (which == 1 ? (void *)&s6->a[0][0] : (void *)&s7->a[0][0]); /* { dg-error "address of scalar with reverse storage order" } */
}

void *addr10 (int which, struct S6 *s6, struct S7 *s7)
{
  return (which == 1 ? (void *)&s6->s2 : (void *)&s7->s1); /* ok */
}
