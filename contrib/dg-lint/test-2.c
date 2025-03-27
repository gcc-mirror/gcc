/* Bad directive ordering; see
   https://gcc.gnu.org/onlinedocs/gccint/Directives.html
   "This directive must appear after any dg-do directive in the test and
   before any dg-additional-sources directive." for
   dg-require-effective-target.  */

/* { dg-require-effective-target c++11 } */
/* { dg-do compile } */
