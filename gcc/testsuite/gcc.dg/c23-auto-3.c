/* Test C23 auto.  Invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

auto; /* { dg-error "empty declaration" } */
auto *p = (int *) 0; /* { dg-error "plain identifier" } */
auto i; /* { dg-error "initialized data declaration" } */
auto g { } /* { dg-error "initialized data declaration" } */
auto a = 1, b = 2; /* { dg-error "single declarator" } */
auto long e0 = 0; /* { dg-error "file-scope declaration" } */
long auto e1 = 0; /* { dg-error "file-scope declaration" } */
int auto e2 = 0; /* { dg-error "file-scope declaration" } */

extern int e3;
auto e3 = 1; /* { dg-error "underspecified declaration of 'e3', which is already declared in this scope" } */

void
f ()
{
  extern int fe1;
  auto fe1 = 1; /* { dg-error "underspecified declaration of 'fe1', which is already declared in this scope" } */
  /* { dg-error "declaration of 'fe1' with no linkage follows extern declaration" "linkage error" { target *-*-* } .-1 } */
  auto fe2 = (struct s *) 0; /* { dg-error "declared in underspecified object initializer" } */
  auto fe3 = (union u *) 0; /* { dg-error "declared in underspecified object initializer" } */
  auto fe4 = (struct s2 { int a; }) { }; /* { dg-error "defined in underspecified object initializer" } */
  auto fe5 = (struct { int a; }) { }; /* { dg-error "defined in underspecified object initializer" } */
  auto fe6 = (union u2 { int a; }) { }; /* { dg-error "defined in underspecified object initializer" } */
  auto fe7 = (union { int a; }) { }; /* { dg-error "defined in underspecified object initializer" } */
  auto fe8 = sizeof (enum e { A }); /* { dg-error "defined in underspecified object initializer" } */
  /* The following case is undefined behavior (so doesn't actually require a
     diagnostic).  */
  auto fe9 = sizeof (enum { B }); /* { dg-error "defined in underspecified object initializer" } */
  /* Examples with a forward declaration, then definition inside auto.  */
  struct s3;
  auto fe10 = (struct s3 { int a; }) { }; /* { dg-error "defined in underspecified object initializer" } */
  union u3;
  auto fe11 = (union u3 { int a; }) { }; /* { dg-error "defined in underspecified object initializer" } */
}

void f2 (auto x); /* { dg-error "storage class specified for parameter" } */
void f3 (auto y) { } /* { dg-error "storage class specified for parameter" } */

auto e4 = sizeof (e4); /* { dg-error "underspecified 'e4' referenced in its initializer" } */
__SIZE_TYPE__ e5;
void
f4 ()
{
  auto e5 = sizeof (e5); /* { dg-error "underspecified 'e5' referenced in its initializer" } */
}

auto typedef int T; /* { dg-error "'typedef' used with 'auto'" } */
auto auto e6 = 1; /* { dg-error "duplicate 'auto'" } */
static auto int e7 = 1; /* { dg-error "multiple storage classes in declaration specifiers" } */
_Thread_local auto int e8 = 2; /* { dg-error "'_Thread_local' used with 'auto'" } */
_Thread_local int auto e9 = 3; /* { dg-error "'_Thread_local' used with 'auto'" } */
/* { dg-error "file-scope declaration of 'e9' specifies 'auto'" "file-scope error" { target *-*-* } .-1 } */

typedef auto int T2; /* { dg-error "multiple storage classes in declaration specifiers" } */

void
f5 ()
{
  static int auto e10 = 3; /* { dg-error "multiple storage classes in declaration specifiers" } */
}

void
f6 ()
{
  static auto l = { 0L }; /* { dg-error "expected expression" } */
  const auto i3 [[]] = { 4, }; /* { dg-error "expected expression" } */
}
