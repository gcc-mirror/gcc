/* Test wrong usage of C23 nullptr.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wall -Wextra -Wno-unused-variable" } */

typedef __typeof__(nullptr) nullptr_t;

void g (nullptr_t); /* { dg-message "expected .nullptr_t. but argument is of type .int." } */
nullptr_t cmp (void);

void
test1 (int *p)
{
  (void) (p > nullptr); /* { dg-error "ordered comparison" } */
  (void) (p >= nullptr); /* { dg-error "ordered comparison" } */
  (void) (p < nullptr); /* { dg-error "ordered comparison" } */
  (void) (p <= nullptr); /* { dg-error "ordered comparison" } */
  (void) (nullptr == 1); /* { dg-error "invalid operands" } */
  (void) (1 == nullptr); /* { dg-error "invalid operands" } */
  (void) (nullptr != 1); /* { dg-error "invalid operands" } */
  (void) (1 != nullptr); /* { dg-error "invalid operands" } */
  (void) (1 > nullptr); /* { dg-error "invalid operands" } */

  /* "(nullptr_t)nullptr" has type nullptr_t but isn't an NPC.  */
  (void) ((nullptr_t)nullptr == p); /* { dg-error "invalid operands" } */
  (void) ((nullptr_t)nullptr != p); /* { dg-error "invalid operands" } */
  (void) (p == (nullptr_t)nullptr); /* { dg-error "invalid operands" } */
  (void) (p != (nullptr_t)nullptr); /* { dg-error "invalid operands" } */
  (void) (cmp () == p); /* { dg-error "invalid operands" } */
  (void) (cmp () != p); /* { dg-error "invalid operands" } */
  (void) (p == cmp ()); /* { dg-error "invalid operands" } */
  (void) (p != cmp ()); /* { dg-error "invalid operands" } */
  /* "(void *)nullptr" is not an NPC, either.  */
  (void) ((void *)nullptr == cmp ()); /* { dg-error "invalid operands" } */
  (void) ((void *)nullptr != cmp ()); /* { dg-error "invalid operands" } */
  (void) (cmp () == (void *)nullptr); /* { dg-error "invalid operands" } */
  (void) (cmp () != (void *)nullptr); /* { dg-error "invalid operands" } */
}

void
test2 (void)
{
  const nullptr_t nptr = nullptr;
  int p = nullptr; /* { dg-error "incompatible types" } */
  float d = nullptr; /* { dg-error "incompatible types" } */
  char arr[10] = { nullptr }; /* { dg-error "incompatible types" } */

  /* No type other than nullptr_t shall be converted to nullptr_t.  */
  const nullptr_t n = 0; /* { dg-error "invalid initializer" } */
  +(nullptr_t) 0; /* { dg-error "conversion from .int. to .nullptr_t." } */

  g (0); /* { dg-error "incompatible type" } */

  int i = 42 + nullptr; /* { dg-error "invalid operands" } */

  /* The assignment of an object of type nullptr_t with a value of another
     type, even if the value is a null pointer constant, is a constraint
     violation.  */
  nullptr_t m;
  m = 0; /* { dg-error "incompatible types" } */
  (void) m;
  nullptr_t o = 0; /* { dg-error "invalid initializer" } */

  switch (nullptr); /* { dg-error "switch quantity not an integer" } */
}

/* If a second or third operand of type nullptr_t is used that is not a null
   pointer constant and the other operand is not a pointer or does not have
   itself nullptr_t, a constraint is violated even if that other operand is
   a null pointer constant such as 0.  */
void
test3 (_Bool b, int i)
{
  const nullptr_t nptr = nullptr;
  __auto_type a1 = b ? nptr : i; /* { dg-error "type mismatch" } */
  __auto_type a2 = b ? i : nptr; /* { dg-error "type mismatch" } */
  __auto_type a3 = b ? nptr : 0; /* { dg-error "type mismatch" } */
  __auto_type a4 = b ? 0 : nptr; /* { dg-error "type mismatch" } */
  __auto_type a5 = b ? 0 : nullptr; /* { dg-error "type mismatch" } */
  __auto_type a6 = b ? nullptr : 0; /* { dg-error "type mismatch" } */
}
