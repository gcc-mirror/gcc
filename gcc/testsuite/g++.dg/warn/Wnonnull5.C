/* PR c++/86568 - -Wnonnull warnings should highlight the relevant argument
   not the closing parenthesis.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define NONNULL __attribute__ ((nonnull))

#if __cplusplus < 201103L
#  define nullptr __null
#endif

struct S
{
  void
  f0 (const void*) const;         // { dg-message "in a call to non-static member function 'void S::f0\\(const void\\*\\) const'" }

  void
  f1 (const void*) const;         // { dg-message "in a call to non-static member function 'void S::f1\\(const void\\*\\) const'" }

  void
  f2 (const void*) const;         // { dg-message "in a call to non-static member function 'void S::f2\\(const void\\*\\) const'" }

  NONNULL void
  f3 (const void*, const void*);  // { dg-message "in a call to function 'void S::f3\\(const void\\*, const void\\*\\)' declared 'nonnull'" }

  NONNULL void
  f4 (const void*, const void*);  // { dg-message "in a call to function 'void S::f4\\(const void\\*, const void\\*\\)' declared 'nonnull'" }

  NONNULL void
  f5 (const void*, const void*);  // { dg-message "in a call to function 'void S::f5\\\(const void\\*, const void\\*\\)' declared 'nonnull'" }

  NONNULL void
  f6 (const void*, const void*);  // { dg-message "in a call to function 'void S::f6\\\(const void\\*, const void\\*\\)' declared 'nonnull'" }
};

void warn_nullptr_this ()
{
  ((S*)nullptr)->f0 ("");        // { dg-warning "3:'this' pointer null" "pr86568" { xfail *-*-* } }
                                 // { dg-warning "this' pointer null" "pr86568" { target *-*-* } .-1 }
}

void warn_null_this_cst ()
{
  S* const null = 0;
  null->f1 ("");                  // { dg-warning "3:'this' pointer null" }
}

void warn_null_this_var ()
{
  S* null = 0;
  null->f2 (&null);               // { dg-warning "3:'this' pointer null" "pr86568" { xfail *-*-* } }
                                  // { dg-warning "'this' pointer null" "pr86568" { target *-*-* } .-1 }
}

void warn_nullptr (S s)
{
  s.f3 (nullptr, &s);              // { dg-warning "9:argument 1 null where non-null expected" "pr86568" { xfail *-*-* } }
                                   // { dg-warning "argument 1 null where non-null expected" "pr86568" { target *-*-* } .-1 }
  s.f3 (&s, nullptr);              // { dg-warning "13:argument 2 null where non-null expected" "pr86568" { xfail *-*-* } }
                                   // { dg-warning "argument 2 null where non-null expected" "pr86568" { target *-*-* } .-1 }
}


void warn_null_cst (S s)
{
  void* const null = 0;
  s.f4 (null, &s);                 // { dg-warning "9:argument 1 null where non-null expected" }
  s.f4 (&s, null);                 // { dg-warning "13:argument 2 null where non-null expected" }
}

void warn_null_var (S s)
{
  void* null = 0;
  s.f5 (null, &s);                // { dg-warning "9:argument 1 null where non-null expected" "pr86568" { xfail *-*-* } }
                                  // { dg-warning "argument 1 null where non-null expected" "pr86568" { target *-*-* } .-1 }
  s.f5 (&s, null);                // { dg-warning "16:argument 2 null where non-null expected" "pr86568" { xfail *-*-* } }
                                  // { dg-warning "argument 2 null where non-null expected" "pr86568" { target *-*-* } .-1 }
}

void warn_null_cond (S s, void *null)
{
  if (null)
    return;

  s.f6 (null, &s);                // { dg-warning "9:argument 1 null where non-null expected" "pr86568" { xfail *-*-* } }
                                  // { dg-warning "argument 1 null where non-null expected" "pr86568" { target *-*-* } .-1 }
  s.f6 (&s, null);                // { dg-warning "13:argument 2 null where non-null expected" "pr86568" { xfail *-*-* } }
                                  // { dg-warning "argument 2 null where non-null expected" "pr86568" { target *-*-* } .-1 }
}


typedef NONNULL void Fvp (const void*, const void*);

void warn_fptr_null_cst (Fvp *p)
{
  void* const null = 0;
  p (null, "");                   // { dg-warning "6:argument 1 null where non-null expected" }
  p ("", null);                   // { dg-warning "10:argument 2 null where non-null expected" }
}

typedef NONNULL void (S::*SMemFvp) (const void*, const void*);

void warn_memfptr_null_cst (S *p, SMemFvp pmf)
{
  void* const null = 0;
  (p->*pmf) (null, "");           // { dg-warning "14:argument 1 null where non-null expected" }
  (p->*pmf) ("", null);           // { dg-warning "18:argument 2 null where non-null expected" }
}
