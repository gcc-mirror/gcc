/* PR middle-end/81824 - Warn for missing attributes with function aliases
   Exercise attribute copy for variables.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define ATTR(list)   __attribute__ (list)

/* Verify that referencing a symbol with no attributes is accepted
   with no diagnostics.  */

int ref0;

ATTR ((copy (ref0))) long
var0;

/* Verify that referencing a symbol using the address-of and dereferencing
   operators is also accepted with no diagnostics.  */

ATTR ((copy (&ref0))) void* ptr0;
ATTR ((copy (*&ref0))) int arr[1];

/* Verify that referencing a symbol of a different kind than that
   of the one the attribute is applied to is diagnosed.  */

int ref1;                     /* { dg-message "previous declaration here" } */

ATTR ((copy (ref1))) int
ref1;                         /* { dg-warning ".copy. attribute ignored on a redeclaration of the referenced symbol " } */


/* Verify that circular references of the copy variable attribute
   are handled gracefully (i.e., not by getting into an infinite
   recursion) by issuing a diagnostic.  */

char xref1;
ATTR ((copy (xref1))) char
xref1;                        /* { dg-warning ".copy. attribute ignored on a redeclaration of the referenced symbol" } */
ATTR ((copy (xref1))) char
xref1;                        /* { dg-warning ".copy. attribute ignored on a redeclaration of the referenced symbol" } */
ATTR ((copy (xref1), copy (xref1))) char
xref1;                        /* { dg-warning ".copy. attribute ignored on a redeclaration of the referenced symbol" } */


/* Use attribute unused to verify that circular references propagate
   atttibutes as expected (expect no warnings the circular reference
   or for any of the unused symbols).  Also use the address-of operator
   to make sure it doesn't change anything.  */

static ATTR ((unused))        int xref2;
static ATTR ((copy (xref2)))  int xref3;
static ATTR ((copy (&xref3))) int xref4;
static ATTR ((copy (xref4)))  int xref5;
static ATTR ((copy (&xref5))) int xref6;
static ATTR ((copy (xref6)))  int xref7;
static ATTR ((copy (&xref7))) int xref8;
static ATTR ((copy (xref8)))  int xref9;
static ATTR ((copy (&xref9))) int xref2;

/* Verify that attribute exclusions apply.  */

ATTR ((common)) int common_var;
ATTR ((nocommon)) double nocommon_var;

ATTR ((copy (common_var), copy (nocommon_var))) long
common_copy;                  /* { dg-warning "ignoring attribute .nocommon. because it conflicts with attribute .common." } */


/* Verify that attribute deprecated isn't copied.  */

ATTR ((deprecated)) char deprecated_var;

ATTR ((copy (deprecated_var))) int current_var;  /* { dg-warning "\\\[-Wdeprecated-declarations]" } */
ATTR ((copy (current_var))) int current_var_2;

int return_current_vars (void) { return current_var + current_var_2; }
