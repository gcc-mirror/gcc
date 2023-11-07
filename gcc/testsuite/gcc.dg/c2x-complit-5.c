/* Test C23 storage class specifiers in compound literals: invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int *p = &(register int) { 0 }; /* { dg-error "file-scope compound literal specifies" } */

int v;

void
f ()
{
  int *q = &(thread_local int) { 0 }; /* { dg-error "compound literal implicitly auto and declared" } */
  int *pc = &(static int) { v }; /* { dg-error "not constant" } */
  int *pt = &(static thread_local int) { v }; /* { dg-error "not constant" } */
  &(register int) { 0 }; /* { dg-error "address of register compound literal requested" } */
  struct s { int a, b; };
  &((register struct s) { 1, 2 }.b); /* { dg-error "address of register compound literal requested" } */
}

int *s = &(static static int) { 0 }; /* { dg-error "duplicate" } */

void
g ()
{
  (void) (register register int) { 0 }; /* { dg-error "duplicate" } */
  (void) (static static int) { 0 }; /* { dg-error "duplicate" } */
  (void) (static thread_local thread_local int) { 0 }; /* { dg-error "duplicate" } */
  (void) (static register int) { 0 }; /* { dg-error "multiple storage classes in declaration specifiers" } */
  (void) (register static int) { 0 }; /* { dg-error "multiple storage classes in declaration specifiers" } */
  (void) (register thread_local int) { 0 }; /* { dg-error "used with" } */
  (void) (thread_local register int) { 0 }; /* { dg-error "used with" } */
}

void
h ()
{
  /* The following cases are not part of the C23 syntax, but are detected
     specially by the parser.  */
  (static int) 0; /* { dg-error "storage class specifier in cast" } */
  sizeof (static int); /* { dg-error "storage class specifier in" } */
  alignof (static int); /* { dg-error "storage class specifier in" } */
}

void
bad_scspec ()
{
  /* Storage class specifiers not permitted in compound literals result in a
     syntax error.  */
  (typedef int) { 0 }; /* { dg-error "expected" } */
  (auto int) { 0 }; /* { dg-error "expected" } */
  (extern int) { 0 }; /* { dg-error "expected" } */
}
