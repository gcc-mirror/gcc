/* PR middle-end/81824 - Warn for missing attributes with function aliases
   Exercise error handling for attribute copy.
   { dg-do compile }
   { dg-require-alias "" }
   { dg-options "-O2 -Wall" } */

#define ATTR(list)   __attribute__ (list)

/* Verify incorrect numbers of arguments.  */
ATTR ((copy)) void
fno_args (void);              /* { dg-error "wrong number of arguments specified for .copy. attribute" } */

ATTR ((copy ())) void
fno_args2 (void);             /* { dg-error "wrong number of arguments specified for .copy. attribute" } */

ATTR ((copy (fno_args, fno_args))) void
fmlti_args (void);            /* { dg-error "wrong number of arguments specified for .copy. attribute" } */

/* Verify that referencing an undeclared symbol is rejected with an error.  */

ATTR ((copy (foobar)))        /* { dg-error ".foobar. undeclared" } */
void fundeclared (void);

/* Verify that using a string argument triggers a descriptive error
   (given attributes like alias and weakref using a string is a likely
   mistake).  */

ATTR ((copy ("foobar")))
void fstring (void);          /* { dg-error ".copy. attribute argument cannot be a string" } */

/* Ditto for an integer.  */

ATTR ((copy (123)))
void fnumber (void);          /* { dg-error ".copy. attribute argument cannot be a constant arithmetic expression" } */
