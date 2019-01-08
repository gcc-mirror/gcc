/* PR middle-end/81824 - Warn for missing attributes with function aliases
   Exercise attribute copy for functions.
   { dg-do compile }
   { dg-require-alias "" }
   { dg-options "-O2 -Wall" } */

#define Assert(expr)   typedef char AssertExpr[2 * !!(expr) - 1]

#define ATTR(list)   __attribute__ (list)

/* Verify that referencing a symbol with no attributes is accepted
   with no diagnostics.  */

void ref0 (void);

ATTR ((copy (ref0))) void
f0 (void);

/* Verify that referencing a symbol using the address-of and dereferencing
   operators is also accepted with no diagnostics.  */

ATTR ((copy (&ref0))) void f1 (void);
ATTR ((copy (*ref0))) void f2 (void);

/* Verify that referencing a symbol of a different kind than that
   of the one the attribute is applied to is diagnosed.  */

int v0;                       /* { dg-message "symbol .v0. referenced by .f3. declared here" } */

ATTR ((copy (v0))) void
f3 (void);                    /* { dg-warning ".copy. attribute ignored on a declaration of a different kind than referenced symbol" } */

void f4 (void);              /* { dg-message "symbol .f4. referenced by .v1. declared here" } */

ATTR ((copy (f4))) int
v1;                           /* { dg-warning ".copy. attribute ignored on a declaration of a different kind than referenced symbol" } */


ATTR ((copy (v0 + 1)))
void f5 (void);               /* { dg-warning ".copy. attribute ignored on a declaration of a different kind than referenced symbol" } */

void f6 (void);

ATTR ((copy (f6 - 1)))
int v1;                       /* { dg-warning ".copy. attribute ignored on a declaration of a different kind than referenced symbol" } */



/* Verify that circular references of the copy function attribute
   are handled gracefully (i.e., not by getting into an infinite
   recursion) by issuing a diagnostic.  */

void xref1 (void);            /* { dg-message "previous declaration here" } */
ATTR ((copy (xref1))) void
xref1 (void);                 /* { dg-warning ".copy. attribute ignored on a redeclaration of the referenced symbol" } */
ATTR ((copy (xref1))) void
xref1 (void);                 /* { dg-warning ".copy. attribute ignored on a redeclaration of the referenced symbol" } */
ATTR ((copy (xref1), copy (xref1))) void
xref1 (void);                 /* { dg-warning ".copy. attribute ignored on a redeclaration of the referenced symbol" } */


/* Use attribute noreturn to verify that circular references propagate
   atttibutes as expected, and unlike in the self-referential instances
   above, without a warning.  Also use the address-of operator to make
   sure it doesn't change anything.  */

ATTR ((noreturn))      void xref2 (void);
ATTR ((copy (xref2)))  void xref3 (void);
ATTR ((copy (&xref3))) void xref4 (void);
ATTR ((copy (xref4)))  void xref5 (void);
ATTR ((copy (&xref5))) void xref6 (void);
ATTR ((copy (xref6)))  void xref7 (void);
ATTR ((copy (&xref7))) void xref8 (void);
ATTR ((copy (xref8)))  void xref9 (void);
ATTR ((copy (&xref9))) void xref2 (void);

int call_ref2 (void) { xref2 (); }
int call_ref3 (void) { xref3 (); }
int call_ref4 (void) { xref4 (); }
int call_ref5 (void) { xref5 (); }
int call_ref6 (void) { xref6 (); }
int call_ref7 (void) { xref7 (); }
int call_ref8 (void) { xref8 (); }
int call_ref9 (void) { xref9 (); }


/* Verify that copying attributes from multiple symbols into one works
   as expected.  */

ATTR ((malloc)) void*
xref10 (void);

ATTR ((alloc_size (1)))
void* xref11 (int);

ATTR ((copy (xref10), copy (xref11)))
void* xref12 (int);

void* call_xref12 (void)
{
  void *p = xref12 (3);
  __builtin___strcpy_chk (p, "123", __builtin_object_size (p, 0));   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
  return p;
}


/* Verify that attribute exclusions apply.  */

ATTR ((const)) int
fconst (void);

ATTR ((pure)) int
fpure (void);                 /* { dg-message "previous declaration here" } */

ATTR ((copy (fconst), copy (fpure))) int
fconst_pure (void);           /* { dg-warning "ignoring attribute .pure. because it conflicts with attribute .const." } */


/* Also verify that the note in the exclusion warning points to
   the declaration from which the conflicting attribute is copied.
   The wording in the note could be improved but it's the same as
   in ordinary exclusions so making it different between the two
   would take some API changes.  */

ATTR ((const)) int
gconst (void);                /* { dg-message "previous declaration here" } */

ATTR ((pure, copy (gconst))) int
gpure_const (void);           /* { dg-warning "ignoring attribute .const. because it conflicts with attribute .pure." } */


/* Verify that attribute deprecated isn't copied (but referencing
   the deprecated declaration still triggers a warning).  */

ATTR ((deprecated)) void
fdeprecated (void);           /* { dg-message "declared here" } */

/* Unlike in most other instance the warning below is on the line
   with the copy attribute that references the deprecated function.  */
ATTR ((copy (fdeprecated)))   /* { dg-warning "\\\[-Wdeprecated-declarations]" } */
int fcurrent (void);

ATTR ((copy (fcurrent))) int
fcurrent2 (void);

int call_fcurrent (void) { return fcurrent () + fcurrent2 (); }


/* Verify that attributes are copied on a declaration using __typeof__
   and that -Wmissing-attributes is not issued.  */

ATTR ((cold)) int
target_cold (void)
{ return 0; }

__typeof__ (target_cold) ATTR ((copy (target_cold), alias ("target_cold")))
alias_cold;                   /* { dg-bogus "\\\[-Wmissing-attributes]." } */


/* Verify that attribute alias is not copied.  This also indirectly
   verifies that attribute copy itself isn't copied.  */

ATTR ((noreturn)) void fnoret (void) { __builtin_abort (); }
ATTR ((alias ("fnoret"), copy (fnoret))) void fnoret_alias (void);
ATTR ((copy (fnoret_alias))) void fnoret2 (void) { __builtin_exit (1); }

/* Expect no warning below.  */
int call_noret (void) { fnoret2 (); }


/* Verify that attribute nonnull (which is part of a function type,
   even when it's specified on a function declaration) is copied to
   the alias from its target.  Expect no warning about the alias
   specifying less restrictive attributes than its target, but do
   verify that passing a null to the alias triggers -Wnonnull.  */

ATTR ((nonnull))
void* ftarget_nonnull (void *p) { return p; }

ATTR ((alias ("ftarget_nonnull"), copy (ftarget_nonnull)))
void* falias_nonnull (void*);

void call_falias_nonnull (void)
{
  falias_nonnull (0);         /* { dg-warning "-Wnonnull" } */
}

/* Same as above but for malloc.  Also verify that the attribute
   on the alias is used by -Wstringop-overflow.  */

ATTR ((malloc))
void* ftarget_malloc (void) { return __builtin_malloc (1); }

ATTR ((alias ("ftarget_malloc"), copy (ftarget_malloc)))
void* falias_malloc (void);

void* call_falias_malloc (void)
{
  char *p = falias_malloc ();
  __builtin___strcpy_chk (p, "123", __builtin_object_size (p, 0));   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
  return p;
}

/* Same as above but for nothrow.  */

ATTR ((nothrow))
void ftarget_nothrow (void) { }

ATTR ((alias ("ftarget_nothrow"), copy (ftarget_nothrow)))
void falias_nothrow (void);
