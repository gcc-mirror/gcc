/* Test spelling differences in UCNs are properly diagnosed for macro
   redefinitions.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c99 -pedantic-errors" } */

/* Different spelling of UCN in expansion.  */
#define m1 \u00c1 /* { dg-message "previous definition" } */
#define m1 \u00C1 /* { dg-error "redefined" } */

#define m1ok \u00c1
#define m1ok \u00c1

/* Different spelling of UCN in argument name.  */
#define m2(\u00c1) /* { dg-message "previous definition" } */
#define m2(\u00C1) /* { dg-error "redefined" } */

#define m2ok(\u00c1)
#define m2ok(\u00c1)

/* Same spelling in argument name but different spelling when used in
   expansion.  */
#define m3(\u00c1) \u00c1 /* { dg-message "previous definition" } */
#define m3(\u00c1) \u00C1 /* { dg-error "redefined" } */

#define m3ok(\u00c1) \u00C1
#define m3ok(\u00c1) \u00C1

/* Different spelling of the macro name itself is OK.  */
#define m4ok\u00c1
#define m4ok\u00C1
