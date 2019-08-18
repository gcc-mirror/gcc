/* Test to verify that a function that returns either the address
   of a local variable or a non-local via a MAX_EXPR or MIN_EXPR
   doesn't return null when the result of the expression is
   the latter.  */

#define NOIPA __attribute__ ((noclone, noinline, noipa))

#define A(expr)                                                 \
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("assertion failed on line %i: %s\n",    \
                        __LINE__, #expr),                       \
      __builtin_abort ()))


typedef __UINTPTR_TYPE__ uintptr_t;

/* Return a bigger value than P.  The address still points (just
   past) the local variable pointed to by P so the caller does
   return the address of a local variable but that's hidden from
   GCC by the attribute and the point of the test is to verify
   that the address in the return statement in the caller isn't
   replaced by null when GCC cannot prove the address doesn't
   reference a non-local variable.  */

NOIPA char* get_max_2 (char *p)
{
  return p + 1;
}

NOIPA char* get_max_3 (char *p, char *q)
{
  return p < q ? q + 1 : p + 1;
}

/* Analogous to the above.  The expressions are undefined because
   they form an address prior to the beginning of the object but
   it's hidden from GCC by the attributes.  */

NOIPA char* get_min_2 (char *p)
{
  return p - 1;
}

NOIPA char* get_min_3 (char *p, char *q)
{
  return p < q ? p - 1 : q - 1;
}


NOIPA void* test_max_2 (void)
{
  char c;

  char *p = get_max_2 (&c);

  void *q = p > &c ? p : &c;  /* MAX_EXPR */
  return q;
}

NOIPA void* test_max_3 (void)
{
  char c;
  char d;

  char *p = get_max_3 (&c, &d);

  void *q = p < &c ? &c < &d ? &d : &c : p;
  return q;
}

NOIPA void* test_min_2 (void)
{
  char c;

  char *p = get_min_2 (&c);

  void *q = p < &c ? p : &c;  /* MIN_EXPR" */
  return q;
}

NOIPA void* test_min_3 (void)
{
  char c;
  char d;

  char *p = get_min_3 (&c, &d);

  void *q = p > &c ? &c > &d ? &d : &c : p;
  return q;
}

NOIPA void* test_min_3_phi (int i)
{
  char a, b;

  char *p0 = &a;
  char *p1 = &b;
  char *p2 = get_min_3 (&a, &b);
  char *p3 = get_min_3 (&a, &b);

  char *p4 = p2 < p0 ? p2 : p0;
  char *p5 = p3 < p1 ? p3 : p1;

  __builtin_printf ("%p %p %p %p\n", p2, p3, p4, p5);

  if (i == 1)
    return p4;
  else
    return p5;
}

int main ()
{
  A (0 != test_max_2 ());
  A (0 != test_max_3 ());

  A (0 != test_min_2 ());
  A (0 != test_min_3 ());

  A (0 != test_min_3_phi (0));
}
