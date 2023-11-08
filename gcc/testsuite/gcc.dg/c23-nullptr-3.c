/* Test wrong usage of C23 nullptr.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wall -Wextra -Wno-unused-variable" } */

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
}

void
test2 (void)
{
  const nullptr_t nptr = nullptr;
  int p = nullptr; /* { dg-error "incompatible types" } */
  float d = nullptr; /* { dg-error "incompatible types" } */
  char arr[10] = { nullptr }; /* { dg-error "incompatible types" } */

  /* Unary '+' is not valid for nullptr.  */
  +nullptr; /* { dg-error "wrong type argument to unary plus" } */

  g (0.0); /* { dg-error "incompatible type" } */
  g (1); /* { dg-error "incompatible type" } */
  g ((int) (float) 0.0); /* { dg-error "incompatible type" } */
  int i = 42 + nullptr; /* { dg-error "invalid operands" } */

  /* The assignment of an object of type nullptr_t with a value of another
     type, other than a null pointer constant, is a constraint
     violation.  */
  nullptr_t m;
  m = 1; /* { dg-error "incompatible types" } */
  m = 0.0; /* { dg-error "incompatible types" } */
  (void) m;
  nullptr_t o = 1; /* { dg-error "incompatible types" } */
  (nullptr_t) 0.0; /* { dg-error "conversion" } */
  (nullptr_t) (int) (float) 0.0; /* { dg-error "conversion" } */

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
