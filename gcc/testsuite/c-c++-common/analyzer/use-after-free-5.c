struct A { __UINT32_TYPE__ x; __UINT32_TYPE__ y; };

int
test_read_off0 (void)
{
  struct A *a = (struct A *) __builtin_malloc (sizeof (struct A));
  if (!a) return 0;
  a->x = 1; a->y = 2;
  __UINT32_TYPE__ *p = &a->x;
  __builtin_free (a); /* { dg-message "freed here" } */
  return *p; /* { dg-warning "use after 'free' of 'p'" } */
}

int
test_read_off4 (void)
{
  struct A *a = (struct A *) __builtin_malloc (sizeof (struct A));
  if (!a) return 0;
  a->x = 1; a->y = 2;
  __UINT32_TYPE__ *p = &a->y;
  __builtin_free (a); /* { dg-message "freed here" "" { xfail *-*-* } } */
  return *p; /* { dg-warning "use after 'free' of 'p'" } */
}

int
test_read_base (void)
{
  struct A *a = (struct A *) __builtin_malloc (sizeof (struct A));
  if (!a) return 0;
  a->x = 1; a->y = 2;
  __builtin_free (a); /* { dg-message "freed here" } */
  return a->y; /* { dg-warning "use after 'free' of 'a'" } */
}

void
test_write_off0 (void)
{
  struct A *a = (struct A *) __builtin_malloc (sizeof (struct A));
  if (!a) return;
  __UINT32_TYPE__ *p = &a->x;
  __builtin_free (a); /* { dg-message "freed here" } */
  *p = 2;  /* { dg-warning "use after 'free' of 'p'" } */
}

void
test_write_off4 (void)
{
  struct A *a = (struct A *) __builtin_malloc (sizeof (struct A));
  if (!a) return;
  __UINT32_TYPE__ *p = &a->y;
  __builtin_free (a); /* { dg-message "freed here" "" { xfail *-*-* } } */
  *p = 2;  /* { dg-warning "use after 'free' of 'p'" } */
}

void
test_write_base (void)
{
  struct A *a = (struct A *) __builtin_malloc (sizeof (struct A));
  if (!a) return;
  a->x = 1; a->y = 2;
  __builtin_free (a); /* { dg-message "freed here" } */
  a->y = 2; /* { dg-warning "use after 'free' of 'a'" } */
}

