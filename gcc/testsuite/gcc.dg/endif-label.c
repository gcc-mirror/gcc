/* { dg-do preprocess } */
/* { dg-options "-pedantic -Wall" } */

/* You can't get away with this in your own code... */
#ifdef KERNEL
#define foo
#endif KERNEL  /* { dg-warning "text following" "good warning" } */

/* This will provoke a warning because the '3' is an extension.  */
#line 10 "endif-label.c" 3 /* { dg-warning "garbage at end" "#line extension" } */

/* ... but in a system header, it's acceptable.  */
#ifdef KERNEL
#define foo
#endif KERNEL  /* { dg-bogus "text following" "bad warning" } */
